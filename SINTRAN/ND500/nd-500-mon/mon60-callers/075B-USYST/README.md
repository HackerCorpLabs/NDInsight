# USYST

MON 60 subfunction **USYST = 075B** (octal) = **0x3D** = **61** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **CHECK IF CURRENT USER IS USER
SYSTEM**, server handler `ITSTUSER`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Named by subfunction (`USYST`). No operator command name is invented here.

## Handler location

USYST is a privilege gate: it takes **no input parameters** and is issued for its
skip/direct return only. Four call sites.

| Call site | JPL | ptr -> thunk | Enclosing routine | Error (callsite+1) | Success (callsite+2) | Status |
|---|---|---|---|---|---|---|
| **006325** | `JPL I 26` | 006353 -> 146605 (`SAA 75`) | 002662 (fs 000331, main interpreter) | 006326 `JPL I -176` -> ptr 006130 = **002673** | 006327 `LDD 25` (continues) | PROVEN |
| **006411** | `JPL I -36` | 006353 -> 146605 | 002662 | 006412 `JPL I 142` -> ptr 006554 = **002673** | 006413 `LDD 143` | PROVEN |
| **006447** | `JPL I -74` | 006353 -> 146605 | 002662 | 006450 `JPL I 104` -> ptr 006554 = **002673** | 006451 `LDD 111` | PROVEN |
| **073461** | `JPL I 50` | 073531 -> 146605 | 073412 (fs 000223) | 073462 `JPL I 43` -> ptr 073525 = **177327** (LEAVE-value, returns error to caller) | 073463 (continues) | PROVEN |

All four resolve to the single USYST thunk **146605**. The three `006xxx` sites
share the same pointer word `006353`; both `006411`/`006447` error paths share
pointer word `006554` = routine `002673` (the interpreter error reporter).

Thunk bytes (verified): `146605`=`170475` (`SAA 75`), `146606`=`125001`, `146607`=`146244`.

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameters | Skip/Error |
|---|---|---|---|---|
| USYST | 075B / 0x3D / 61 | 146605 | **none** (no `LDX ,B -176` / `STx ,X n` precedes any of the four sites) | success = current user IS SYSTEM; error = NOT system |

## What it does

Each call site issues `MON 60` USYST with no parameter block and branches on the
return. On the SKIP (success) return the user is user SYSTEM and the caller proceeds;
on the DIRECT (error) return the caller diverts to the interpreter error reporter
`002673` (three interpreter sites) or performs a LEAVE-with-value returning an error
to the caller of routine `073412` (`073461`).

USYST guards operations that must be restricted to user SYSTEM.

## Unknown / inferred

- **PROVEN**: no input parameters are marshalled at any of the four call sites (no
  `LDX ,B -176` + `STx ,X n` sequence immediately precedes them).
- **INFERRED**: success = "is SYSTEM", error = "not SYSTEM". The return polarity
  (skip=success, direct=error) is PROVEN from the gateway; that "success" means the
  user IS SYSTEM rests on the NPL purpose string, not a traced server result.
- **INFERRED (role)**: `002673` is the interpreter error reporter; its entry address
  and that the error paths reach it are PROVEN. `177327` is the runtime LEAVE-value
  helper (PROVEN, prog.md sec 4.3).
