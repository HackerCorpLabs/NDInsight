# SLOGA

MON 60 subfunction **SLOGA = 115B** (octal) = **0x4D** = **77** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **START PROCESS-LOG-ALL**, server
handler `ISTLAPR`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Named by subfunction (`SLOGA`). No operator command name is invented here.

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **110055** (framesize 001137) - the process-logging service | PROVEN (ENTER prologue at 110055; also hosts SLOG1/RLOG) |
| MON 60 call site | **110143** `JPL I 120` -> ptr 110263, `bank1[110263]=146654` = thunk `SAA 115` | PROVEN |
| Error path | 110144 (callsite+1) `JPL -54` (relative) | PROVEN |
| Success path | 110145 (callsite+2) `SAA 24` (routine continues) | PROVEN |

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameters | Skip/Error |
|---|---|---|---|---|
| SLOGA | 115B / 0x4D / 77 | 146654 | **none** (no `LDX ,B -176` / `STx ,X n` precedes the call; word `110142` `JMP 13` is a branch, not a param store) | err=110144 (callsite+1); ok=110145 (callsite+2) |

Thunk bytes (verified): `146654`=`170515` (`SAA 115`), `146655`=`125001`, `146656`=`146244`.

## What it does

Issues `MON 60` SLOGA with no parameter block to start logging for ALL processes. On
error -> callsite+1; on success continues.

Contrast with SLOG1 (111B, START PROCESS LOG ONE) in the same routine, which passes
one parameter (`&(B-171)`, the process identity): "log all" needs no argument.

## Unknown / inferred

- **PROVEN**: no input parameters (no parameter-store sequence precedes `110143`).
- **INFERRED (role)**: routine `110055` is the process-logging service (issues SLOGA,
  SLOG1, RLOG). ENTER address and framesize are PROVEN; internals not carved.
