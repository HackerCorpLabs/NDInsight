# 003B-RDATA

MON 60 subfunction **RDATA = 3B** (octal) = **0x03** = **3** decimal.
NPL purpose (authoritative, `SUBFUNCTION-TABLE.md`): **LOGICAL DATA MEMORY READ**.
Server handler: **5NOPAR** (generic forward path).

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

| Addr | Word | Meaning |
|---|---|---|
| 146324 | 170403 | `SAA 3` -> subfunction code 3 |
| 146325 | 125001 | `JMP I 1` |
| 146326 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 022462 | ENTER-routine **022310** (framesize 000014) | `JPL I 52` -> 022534 | `bank1[022534]=146324` | `SAA 3` | PROVEN |
| 055211 | ENTER-routine **055151** (framesize 000013) | `JPL I 7`  -> 055220 | `bank1[055220]=146324` | `SAA 3` | PROVEN |
| 056723 | ENTER-routine **056042** (framesize 000050) | `JPL I 106` -> 057031 | `bank1[057031]=146324` | `SAA 3` | PROVEN |

All three are standalone ENTER routines (not inside the 002662 interpreter).

## Parameter block (X := b.-176; `,X 6/7/10/13`, `,X 10` is a 3-word `STF`)

| Site | `,X 6` | `,X 7` | `,X 10` | `,X 13` |
|---|---|---|---|---|
| 022462 | `&(B-164)` (=const LDD 67) @022450 | const LDA 45 @022452 | F reg @022456 | `&(B-170)` @022461 |
| 055211 | `&(B-165)` @055173 | `&(B-172)` @055176 | F reg @055205 | `&(B-165)` @055210 |
| 056723 | val LDA -4 @056705 | `&(B-163)` @056710 | F reg @056717 | `&(B-165)` @056722 |

## Skip / error handling

- 022462: err 022463 (`JPL -146`->022315, `146147` local block); ok 022464 (`JMP 135`->022621).
- 055211: err 055212 (`JPL I 4`->055216 = `177327` LEAVE-value); ok 055213 (`JPL I 6`->055221 = `177335` LEAVE-skip).
- 056723: err 056724 (`JPL -117`->056605); ok 056725 (`LDX ,B -172`).

## Unknown / inferred

- **INFERRED**: which slot is the logical data address / length / result buffer.
  RDATA "LOGICAL DATA MEMORY READ" implies one slot is a DM address and one a buffer;
  ordering not cross-checked. All store addresses/operands are PROVEN.
- **UNKNOWN**: pool constants (`LDD 67`, `LDA 45/54/-4`) not decoded.
- **PROVEN**: thunk bytes, all three pointer resolutions to 146324, all call sites,
  every listed parameter store, and the callsite+1/+2 targets.
