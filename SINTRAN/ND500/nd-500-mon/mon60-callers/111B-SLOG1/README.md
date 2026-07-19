# SLOG1

MON 60 subfunction **SLOG1 = 111B** (octal) = **0x49** = **73** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **START PROCESS LOG ONE**, server
handler `ISTAPRLOG`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Named by subfunction (`SLOG1`). No operator command name is invented here.

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **110055** (framesize 001137) - a logging service routine | PROVEN (ENTER prologue at 110055) |
| MON 60 call site | **110161** `JPL I 105` -> ptr 110266, `bank1[110266]=146643` = thunk `SAA 111` | PROVEN |
| Error path | 110162 (callsite+1) `JPL -72` (relative) | PROVEN |
| Success path | 110163 (callsite+2) `SAA 15` (routine continues) | PROVEN |

Routine `110055` also hosts RLOG (`110116`/`110243`/`110310`) and SLOGA (`110143`) -
it is the process-logging service.

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| SLOG1 | 111B / 0x49 / 73 | 146643 | `,X 6` = &(B-171) [110155 `RADD SB DA`; 110156 `AAA -171`; 110160 `STA ,X 6`] | err=110162 (callsite+1); ok=110163 (callsite+2) |

Thunk bytes (verified): `146643`=`170511` (`SAA 111`), `146644`=`125001`, `146645`=`146244`.

## What it does

1. Computes `&(B-171)` and stores it into gateway param slot 1. (`110155-110160`)
2. Issues `MON 60` SLOG1 to start logging for ONE process. (`110161`)
3. On error -> callsite+1; on success continues.

Contrast with SLOGA (115B, START PROCESS-LOG-ALL), which takes no parameter: "log one"
needs the process identity, "log all" does not.

## Unknown / inferred

- **PROVEN**: single parameter `,X 6` = &(B-171).
- **INFERRED**: `B-171` identifies the process to log. Not traced; the store and offset
  are PROVEN.
- **INFERRED (role)**: routine `110055` is the process-logging service (issues SLOG1,
  SLOGA and RLOG). ENTER address and framesize are PROVEN; internals not carved.
