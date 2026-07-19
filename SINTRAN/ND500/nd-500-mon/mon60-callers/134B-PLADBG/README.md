# PLADBG (PLACE DEBUGGER)

MON 60 subfunction **PLADBG = 134B** (octal) = **0x5C** = **92** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**PLACE DEBUGGER**, server handler `IPLDEB`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002527** (framesize 000075 = 61 dec) | PROVEN (002527 `RADD AD1 CLD SL DX`; 002530 `JPL I 70` -> ptr `002620`=`177300` ENTER) |
| MON 60 call site | **002560** `JPL I 44` -> ptr `002624`, `bank1[002624]=146726` = thunk `SAA 134` | PROVEN |
| Error path | 002561 (callsite+1) -> ptr `002622` = **177327** LEAVE(value) | PROVEN |
| Success path | 002562 (callsite+2) - continues in-line | PROVEN |

## Thunk verification (PROVEN, read from bytes)

`146726 170534 SAA 134` ; `146727 125001 JMP I 1` ; `146730 146244` (gateway).

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout (main path) | Skip/Error |
|---|---|---|---|---|
| PLADBG | 134B / 0x5C / 92 | 146726 | `,X 6` = `&local(B-167)`; `,X 7` = the 3-word `F` descriptor from input `B-172` | err=002561->177327; ok=002562 in-line |

Slots 6 and 7 are stored (`002555 STA ,X 6`, `002557 STF ,X 7`).

## What it does

1. Places `&local(B-167)` in gateway param slot 1. (`002552-002555`)
2. Loads the debugger descriptor (`F`, 3 words) from input `B-172` into param
   slot 2. (`002556-002557`)
3. Issues `MON 60` PLADBG. (`002560`)
4. On error -> `177327` LEAVE(value); on success -> continues in-line.

There is an alternate branch at `002536-002546` (taken when `local(X+2) < 0`) that
sets param1 := 1 and param2 := `F@(local(B-165)+3)` and then calls **routine
043011** via ptr `002621` - that target is **not** a MON 60 thunk, so it is a
different (non-ND-500) code path, noted but not part of this MON 60 call.

## Unknown / inferred

- **PROVEN**: subfunction identity, thunk bytes, the two adjacent param stores on
  the main path, and the error/success targets.
- **INFERRED**: `B-172` is the debugger domain/file name; `local(B-167)` is an
  output or mode/status word. Exact roles not traced. Server handler `IPLDEB`
  places the debugger.
