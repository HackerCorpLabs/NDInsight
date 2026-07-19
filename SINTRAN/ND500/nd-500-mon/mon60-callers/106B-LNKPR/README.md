# LNKPR

MON 60 subfunction **LNKPR = 106B** (octal) = **0x46** = **70** decimal.
Purpose (`5P-P2-MON60.NPL` / NDInsight): **(link to process)**, server handler
`5NOPAR`. This code has no verbatim `% FUNCTION=` comment in the NPL; the purpose
"(link to process)" is the NDInsight-derived label (parenthesized in
SUBFUNCTION-TABLE.md), so it is INFERRED, not a verbatim server string. The INDEX
lists this as the ATTACH-PROCESS (link) client `LNKPR`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Named by subfunction (`LNKPR`). No operator command name is invented here.

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331) - main command interpreter | PROVEN |
| MON 60 call site | **006711** `JPL I 57` -> ptr 006770, `bank1[006770]=146635` = thunk `SAA 106` | PROVEN |
| Error path | 006712 (callsite+1) `JPL I -136` -> ptr 006554 = routine **002673** | PROVEN |
| Success path | 006713 (callsite+2) `STZ I 56` (routine continues) | PROVEN |

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| LNKPR | 106B / 0x46 / 70 | 146635 | `,X 6` = &(B-117) [006701-006710]; B-117 filled from an evaluated operand (`SAA 0`; `JPL I -127` @006702; `STD ,B -117` @006704) | err=006712 -> 002673; ok=006713 |

Thunk bytes (verified): `146635`=`170506` (`SAA 106`), `146636`=`125001`, `146637`=`146244`.

## What it does

1. Evaluates one command operand into local `B-117` (`006701-006704`).
2. Stores `&(B-117)` into gateway param slot 1. (`006705-006710`)
3. Issues `MON 60` LNKPR. (`006711`)
4. On error -> `002673`; on success continues (`006713 STZ I 56`).

## Unknown / inferred

- **PROVEN**: single parameter `,X 6` = &(B-117), filled from one evaluated operand.
- **INFERRED**: the operand identifies the process to link/attach to. Not traced; the
  store and offset are PROVEN.
- **INFERRED**: the purpose "(link to process)" itself (no verbatim NPL FUNCTION=
  comment for code 106).
- **INFERRED (role)**: `002673` = interpreter error reporter (PROVEN target).
