# ABSMR

MON 60 subfunction **032B** (octal) = **0x1A** = **26** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via SUBFUNCTION-TABLE.md):
**PHYSICAL DATA MEMORY READ**, server handler `5NOPAR`.
Client name (yaml `60B_N500M.yaml`): `ABSMR`, "Absolute memory read",
params `<no. of bytes> <ND-500 addr.> <data area> <bytes returned>`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN)

`146426 SAA 32` / `146427 JMP I 1` / `146430 146244` (gateway). Verified from bytes.

## Call sites (all PROVEN)

| Call site | Enclosing routine | Resolution | Kind |
|---|---|---|---|
| `012721` `JPL I 42` | ENTER **012700** (framesize `000012`=10) | ptr `012763`=`146426` | standalone ENTER |
| `022616` `JPL I 13` | ENTER **022310** (framesize `000014`=12) | ptr `022631`=`146426` | standalone ENTER |
| `056364` `JPL I 75` | ENTER **056042** (framesize `000050`=40) | ptr `056461`=`146426` | standalone ENTER |
| `131163` `JPL I 151` | ENTER **130475** (framesize `000207`=135) | ptr `131334`=`146426` | standalone ENTER |

## Parameter block filled before each call

| Site | `,X 6` (no. of bytes) | `,X 7` (ND-500 addr.) | data area | bytes returned |
|---|---|---|---|---|
| A `012721` | value (`012710`) | `&(B-165)` (`012713`) | `(B-170)` value at `,X 10` (`012715`) | `&(B-163)` at `,X 11` (`012720`) |
| B `022616` | `&(B-160)` (`022604`) | value (`022606`) | F 3-word at `,X 10` (`022612`) | `&(B-170)` at `,X 13` (`022615`) |
| C `056364` | value (`056346`) | `&(B-163)` (`056351`) | F 3-word at `,X 10` (`056360`) | `&(B-165)` at `,X 13` (`056363`) |
| D `131163` | `&(B-66)` (`131151`) | value (`131153`) | F 3-word at `,X 10` (`131157`) | `&(B-110)` at `,X 13` (`131162`) |

PROVEN layout difference: site A passes `<data area>` as a single word (`,X 10`),
so `<bytes returned>` sits at `,X 11`. Sites B/C/D pass a 3-word F descriptor at
`,X 10`, pushing `<bytes returned>` to `,X 13`.

## Skip / error handling

| Site | ERROR (callsite+1) | SUCCESS (callsite+2) |
|---|---|---|
| A `012721` | `012722 JPL I 42` -> ptr `012764` = `177327` LEAVE(value) | continues `012723` |
| B `022616` | `022617 JPL I 4` -> ptr `022623` | `022620 JMP 1` -> `022621` (-> `022512`) |
| C `056364` | `056365 JPL I -125` -> ptr `056240` | `056366 JMP I 64` -> ptr `056452` |
| D `131163` | `131164 JPL I -55` -> `131107` | continues `131165` |

## Unknown / inferred

- INFERRED (semantic): parameter labels follow the yaml order; every store/offset is PROVEN.
- INFERRED (role): the per-routine error/success targets (`012764`, `022623`,
  `056240`/`056452`, `131107`) are the routines' exit/continuation points; not traced.
- The P-relative constants loaded for `<no. of bytes>` / `<ND-500 addr.>` were not read
  back to numeric values (labelled "constant"/"value").
