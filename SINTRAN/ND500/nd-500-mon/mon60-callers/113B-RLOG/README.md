# RLOG

MON 60 subfunction **RLOG = 113B** (octal) = **0x4B** = **75** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **READ LOG DATA (PRINT LOG INFO)**,
server handler `IPRILOG`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Named by subfunction (`RLOG`). No operator command name is invented here.

## Handler location - three call sites

All three lie in the process-logging service routine **110055** (framesize 001137).
Each marshals **two** parameters: a P-relative constant in slot 1 and the F register
(a 3-word descriptor) in slots 2/3/4 via `STF ,X 7`.

| Call site | JPL | ptr -> thunk | `,X 6` | `,X 7` (F, 3 words) | Error (callsite+1) | Success (callsite+2) | Status |
|---|---|---|---|---|---|---|---|
| **110116** | `JPL I 137` | 110255 -> 146646 | const (`LDA 144`) [110111] | F [110115 `STF ,X 7`] | 110117 `JPL -27` | 110120 `STZ ,B -134` | PROVEN |
| **110243** | `JPL I 66`  | 110331 -> 146646 | const (`LDA 40`)  [110236] | F [110242 `STF ,X 7`] | 110244 `JPL -154` | 110245 `SAA -1` | PROVEN |
| **110310** | `JPL I -33` | 110255 -> 146646 | const (`LDA -5`)  [110303] | F [110307 `STF ,X 7`] | 110311 `JPL I 16` -> 110327 = **110070** | 110312 `LDA ,B -117` | PROVEN |

All three resolve to the single RLOG thunk **146646**. Sites `110116` and `110310`
share pointer word `110255`; site `110310`'s error path loops back to `110070`
(routine start region).

Thunk bytes (verified): `146646`=`170513` (`SAA 113`), `146647`=`125001`, `146650`=`146244`.

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| RLOG | 113B / 0x4B / 75 | 146646 | `,X 6` = P-relative constant (selector, role INFERRED); `,X 7` = F register (3-word descriptor) | see per-site table |

## What it does

Each site loads a distinct constant selector into param slot 1 and the F register
(a 3-word descriptor - the standard file-name/buffer form) into slots 2/3/4, then
issues `MON 60` RLOG to read/print log data. The three sites are variants (different
selector constants) within the logging service.

## Unknown / inferred

- **PROVEN**: two logical parameters - one word (`,X 6`) plus the 3-word F register
  (`STF ,X 7`).
- **INFERRED**: `,X 6` is a log-type/selector constant and the F descriptor is a
  file/buffer for the log output. Constant values and descriptor content not traced;
  the stores are PROVEN.
- **INFERRED (role)**: routine `110055` is the process-logging service; `110070` is a
  point in its prologue region reached by the `110310` error loop. Addresses PROVEN,
  internals not carved.
