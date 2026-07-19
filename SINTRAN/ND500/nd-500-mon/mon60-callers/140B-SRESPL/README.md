# SRESPL

MON 60 subfunction **SRESPL = 140B** (octal) = **0x60** = **96** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**UNDOCUMENTED** - table row 140 has dispatch `5NOPAR` and no verbatim
`FUNCTION=` comment. `SRESPL` is only the yaml/thunk client label; no meaning is
invented here.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk verification (PROVEN, read from bytes)

`146737 170540 SAA 140` ; `146740 125001 JMP I 1` ; `146741 146244` (gateway).

## Call sites (2) and enclosing routines

| Call site | Enclosing ENTER-routine | Framesize | Pointer word | Error (callsite+1) | Success (callsite+2) | Status |
|---|---|---|---|---|---|---|
| **043547** `JPL I 125` | **043011** | 000717 | `043674`=146737 | 043550 -> ptr `043675` = routine **043321** | 043551 -> 043554 | PROVEN |
| **063062** `JPL I 67` | **062257** | 000544 | `063151`=146737 | 063063 -> ptr `063140` = routine **062446** | 063064 -> 063067 | PROVEN |

(ENTER pointers verified: `bank1[043203]=bank1[062454]=177300`.)

## Structure: paired with SPLAC (START-PLACE 055B)

At both sites SRESPL is the **flag-set arm** of an if/else; the **flag-clear arm**
issues **SPLAC = 055B (START-PLACE)** instead:

- 043546 `JAZ -> 043552`: flag `local(B-163)`==0 -> SPLAC (ptr `043676`=`146530`); else SRESPL.
- 063061 `JAZ -> 063065`: flag `local(B-165)`==0 -> SPLAC (ptr `063152`=`146530`); else SRESPL.

(`146530 170455 SAA 055` verified = SPLAC.) So 140B lives in a PLACE context.

## MON 60 parameter block

**None marshalled.** No `LDX ,B -176` / `STA ,X n` stores precede either SRESPL
call. Consistent with the `5NOPAR` generic-forward dispatch.

## Unknown / inferred

- **PROVEN**: subfunction identity (140B), thunk bytes, both call sites take no
  marshalled parameters, the error/success targets, and the SPLAC pairing.
- **UNKNOWN**: the meaning of subfunction 140B / `SRESPL`. It is undocumented in
  the NPL source; no name expansion is asserted. Its pairing with `SPLAC`
  (START-PLACE) suggests a PLACE-related reserve/reset operation, but this is
  **not proven**.
