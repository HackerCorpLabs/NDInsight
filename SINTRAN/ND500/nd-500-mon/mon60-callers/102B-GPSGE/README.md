# GPSGE (STOP ND-500 SYSTEM)

MON 60 subfunction **GPSGE = 102B** (octal) = **0x42** = **66** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**STOP ND-500 SYSTEM (ABORT ALL ACTIVE PROCS, RELEASE MON60 BUFFERS)**,
server handler `IFORGET`.

IMPORTANT: subfunction 102B is **not** a status/"get" call. `GPSGE` is only the
thunk/yaml client label; the authoritative action is a system stop.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331) - the command interpreter | PROVEN |
| MON 60 call site | **007514** `JPL I 114` -> ptr `007630`, `bank1[007630]=146673` = thunk `SAA 102` | PROVEN |
| Error path | 007515 (callsite+1) -> **007500** (local error handler) | PROVEN |
| Success path | 007516 (callsite+2) -> ptr `007376` = **010613** (command loop) | PROVEN |

## Thunk verification (PROVEN, read from bytes)

`146673 170502 SAA 102` ; `146674 125001 JMP I 1` ; `146675 146244` (gateway).

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameters | Skip/Error |
|---|---|---|---|---|
| GPSGE | 102B / 0x42 / 66 | 146673 | **none marshalled** (no `,X` slot stores precede the call) | err=007515->007500; ok=007516->010613 |

## What it does

1. Sets two local flags via indirect stores (`007510-007513`: `SAA 1` then
   `STA I 115` / `STA I 114`) - INFERRED to be stop/abort indicators.
2. Issues `MON 60` GPSGE with no input parameters. (`007514`)
3. On error -> local handler `007500`; on success -> command loop `010613`.

The absence of any MON 60 parameter marshalling matches a global "stop the entire
ND-500 system" action rather than a per-process or data-transfer call.

## Unknown / inferred

- **PROVEN**: subfunction identity, thunk bytes, that no MON 60 parameters are
  marshalled, and the error/success targets.
- **INFERRED**: the two `STA I` stores at 007511/007513 set local flags before
  the call; their exact meaning was not traced. The actual system stop / process
  abort / buffer release is performed by the SINTRAN handler `IFORGET`.
