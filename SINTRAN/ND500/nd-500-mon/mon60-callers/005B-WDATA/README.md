# 005B-WDATA

MON 60 subfunction **WDATA = 5B** (octal) = **0x05** = **5** decimal.
NPL purpose (authoritative, `SUBFUNCTION-TABLE.md`): **LOGICAL DATA MEMORY WRITE**.
Server handler: **IDMWRITE** (special input-marshalling handler, not `5NOPAR`).

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

| Addr | Word | Meaning |
|---|---|---|
| 146327 | 170405 | `SAA 5` -> subfunction code 5 |
| 146330 | 125001 | `JMP I 1` |
| 146331 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 002326 | ENTER-routine **002222** (framesize 000004) | `JPL I 145` -> 002473 | `bank1[002473]=146327` | `SAA 5` | PROVEN |
| 002515 | ENTER-routine **002222** (reached by `JMP 27` from 002466) | `JPL I -22` -> 002473 | `bank1[002473]=146327` | `SAA 5` | PROVEN |
| 055140 | ENTER-routine **055113** (framesize 000007) | `JPL I 7` -> 055147 | `bank1[055147]=146327` | `SAA 5` | PROVEN |
| 056023 | ENTER-routine **055255** (framesize 000302) | `JPL I 15` -> 056040 | `bank1[056040]=146327` | `SAA 5` | PROVEN |

All four are standalone ENTER routines. (002222 sits *before* the 002662 interpreter;
it is a distinct routine, not a case of it.)

## Parameter block (X := b.-176; `,X 6/7/10`, `,X 10` is a 3-word `STF`)

| Site | `,X 6` | `,X 7` | `,X 10` |
|---|---|---|---|
| 002326 | `&(B-104)` (=const LDD 162) | `&(B-102)` | F reg (3-word) |
| 002515 | `&(B-104)` (=const LDD 43) | `&(B-110)` | F reg (3-word) |
| 055140 | `&(B-165)` | `&(B-172)` | F reg (`LDF ,B -170`) |
| 056023 | `&(B-170)` | `&(B-166)` | F reg (`LDF ,B -155`) |

056023 uses the identical `&(B-170)/&(B-166)/F(B-155)` layout as 004B-WPROG at 055702
in the same routine 055255 - a matched WPROG/WDATA pair.

## Skip / error handling

- 002326: err 002327 (`JMP I ,B -163` frame-relative dispatch -> 002144); ok 002330.
- 002515: err 002516 (`JMP I ,B -163` -> 002333); ok 002517.
- 055140: err 055141 (`JPL I 4`->055145 = `177327` LEAVE-value); ok 055142 (`JPL I 6`->055150 = `177335` LEAVE-skip).
- 056023: err 056024 (`JPL I 3`->056027 = `177327` LEAVE-value); ok 056025 (`JPL I 14`->056041).

Routine 002222 is itself a mini command-dispatcher: its error return is a computed
`JMP I ,B -163` through a frame-held jump table (target differs per site: 002144, 002333).

## Unknown / inferred

- **INFERRED**: which slot is the logical DM address / count / data. WDATA "LOGICAL DATA
  MEMORY WRITE" and the matched WPROG layout support (do not prove) address/count/data.
  All store addresses/operands PROVEN.
- **UNKNOWN**: pool constants `LDD 162`, `LDD 43`, and the `SAD 33`/`SAD SHR 20`
  unpacked values were not decoded.
- **PROVEN**: thunk bytes, all four pointer resolutions to 146327, all call sites, every
  listed parameter store, and the callsite+1/+2 targets.
