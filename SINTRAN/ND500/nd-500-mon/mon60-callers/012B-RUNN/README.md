# 012B-RUNN

MON 60 subfunction **RUNN = 12B** (octal) = **0x0A** = **10** decimal.
NPL purpose (authoritative, `SUBFUNCTION-TABLE.md`): **START ND-500 PROGRAM**.
Server handler: **5NOPAR** (generic forward path).

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

| Addr | Word | Meaning |
|---|---|---|
| 146346 | 170412 | `SAA 12` -> subfunction code 012 (10 dec) |
| 146347 | 125001 | `JMP I 1` |
| 146350 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 030635 | ENTER-routine **030515** (framesize 000011), standalone | `JPL I 110` -> 030745 | `bank1[030745]=146346` | `SAA 12` | PROVEN |
| 030737 | ENTER-routine **030515**, standalone (params at 030673, `JMP 33`->030737) | `JPL I -3` -> 030734 | `bank1[030734]=146346` | `SAA 12` | PROVEN |

Both are in the same standalone routine 030515; neither is inside the 002662 interpreter.
This routine is the worked example in `nd-500-mon-j04.prog.md` section 5.5.

## Parameter block (X := b.-176; three slots, identical at both sites)

| Slot | Set at (030635 / 030737) | Value | Meaning (yaml signature) |
|---|---|---|---|
| `,X 6` (param1) | 030627 / 030676 | `&(B-167)` | `&<stop reason>` |
| `,X 7` (param2) | 030631 / 030700 | `local(B-171)` | `<clear time used>` (INFERRED order) |
| `,X 10` (param3) | 030634 / 030703 | `&(B-165)` | `&<returned trap info>` |

Three parameters match the documented RUNN signature `<stop reason> <returned trap info>
<clear time used>` (60B_N500M.yaml), confirmed in prog.md 5.5.

## Skip / error handling

- 030635: err 030636 (`JPL -115`->030521, a `146147` local block); ok 030637 (`LDD ,B -167` reads the stop reason).
- 030737: err 030740 (`JPL I 6`->030746 which = 030521); ok 030741 (`LDD ,B -167`).

The second site (030737) is a re-entry: code at 030673-030703 refills the same three
slots then `JMP 33` jumps to the shared RUNN call at 030737 (a second start path).

## Unknown / inferred

- **INFERRED**: mapping of `,X 7` = `<clear time used>` (order in the 3-arg signature).
  `,X 6` and `,X 10` are confirmed as stop-reason and trap-info by the section-5.5 trace.
- **PROVEN**: thunk bytes, both pointer resolutions to 146346, both call sites, all three
  stores at each, and the callsite+1/+2 targets.
