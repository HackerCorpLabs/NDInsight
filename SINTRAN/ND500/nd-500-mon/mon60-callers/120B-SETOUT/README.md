# SETOUT

MON 60 subfunction **SETOUT = 120B** (octal) = **0x50** = **80** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**(set output device)** (no verbatim `FUNCTION=` prose; dispatches to the generic
`5NOPAR` input path).

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331) - the command interpreter | PROVEN (same routine as LOAD-CONTROL-STORE; ENTER at 002662) |
| This command's case | **003556 .. 003576** | PROVEN (bounded by command-continue jumps to 003527) |
| MON 60 call site | **003573** `JPL I 156` -> ptr `003751`, `bank1[003751]=146662` = thunk `SAA 120` | PROVEN |
| Error path | 003574 (callsite+1) -> ptr `003750` = routine **002673** (interpreter error reporter) | PROVEN |
| Success path | 003575 (callsite+2) -> **003527** (command continue) | PROVEN |

## Thunk verification (PROVEN, read from bytes)

`146662 170520 SAA 120` ; `146663 125001 JMP I 1` ; `146664 146244` (gateway).

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout (this handler) | Skip/Error |
|---|---|---|---|---|
| SETOUT | 120B / 0x50 / 80 | 146662 | `,X 6` = `&local(B-127)`, a value from the indirect load at 003564 masked by `SAD SHR 20` (>>16) | err=003574->002673; ok=003575->003527 |

Only slot 6 is stored (`003572 STA ,X 6`).

## What it does

1. Loads a value via the indirect reference at `003564` and keeps the high 16
   bits (`SAD SHR 20`), storing it in local `B-127`. (`003564-003566`)
2. Places `&local(B-127)` in gateway param slot 1 and issues `MON 60` SETOUT. (`003567-003573`)
3. On error -> interpreter error reporter `002673`; on success -> command continue `003527`.

## Unknown / inferred

- **PROVEN**: subfunction identity, thunk bytes, the single param store, error/success targets.
- **INFERRED**: subfunction 120B is "(set output device)" per SUBFUNCTION-TABLE.md
  (handler `5NOPAR`, i.e. the generic forward path). The passed value is the
  output device/unit selector; its exact encoding and the source of the indirect
  load at 003564 were not traced.
