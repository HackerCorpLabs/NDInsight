# SMONLOG

MON 60 subfunction **SMONLOG = 124B** (octal) = **0x54** = **84** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**START MONITOR CALL LOG**, server handler `ISTAMLOG`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331) - the command interpreter | PROVEN |
| MON 60 call site | **007312** `JPL I 72` -> ptr `007404`, `bank1[007404]=146676` = thunk `SAA 124` | PROVEN |
| Error path | 007313 (callsite+1) -> ptr `007171` = routine **002673** (interpreter error reporter) | PROVEN |
| Success path | 007314 (callsite+2) -> ptr `007376` = **010613** (command loop) | PROVEN |

## Thunk verification (PROVEN, read from bytes)

`146676 170524 SAA 124` ; `146677 125001 JMP I 1` ; `146700 146244` (gateway).

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| SMONLOG | 124B / 0x54 / 84 | 146676 | `,X 6` = `&local(B-127)`, a value from helper routine 002222 (fed the filename descriptor F@B-113) masked by `SAD SHR 20` (>>16) | err=007313->002673; ok=007314->010613 |

Only slot 6 is stored (`007311 STA ,X 6`).

## What it does

1. Calls helper routine `002222` (`007302`), passing the filename descriptor
   (`F` register, 3 words, from `B-113`) in slot 6; on error -> `002673`.
2. Keeps the high 16 bits of the returned value (`007304 SAD SHR 20`) into
   local `B-127`. (`007304-007305`)
3. Places `&local(B-127)` in gateway param slot 1 and issues `MON 60` SMONLOG. (`007306-007312`)
4. On error -> `002673`; on success -> command loop `010613`.

## Unknown / inferred

- **PROVEN**: subfunction identity, thunk bytes, the single param store, error/success targets.
- **INFERRED**: helper `002222` resolves the log file/segment; `local(B-127)` is
  the resulting log-buffer descriptor/size. The exact roles were not traced.
  Server handler `ISTAMLOG` starts the monitor-call log on the SINTRAN side.
