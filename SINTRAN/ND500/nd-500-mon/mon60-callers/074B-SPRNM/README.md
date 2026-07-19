# SPRNM

MON 60 subfunction **SPRNM = 074B** (octal) = **0x3C** = **60** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **SET NAME ON CURRENT PROCESS**,
server handler `ISPRNM`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Named by subfunction (`SPRNM`). No operator command name is invented here.

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331) - the main command interpreter | PROVEN (same routine that hosts LOAD-CONTROL-STORE etc.) |
| MON 60 call site | **010112** `JPL I 151` -> ptr 010263, `bank1[010263]=146602` = thunk `SAA 74` | PROVEN |
| Error path | 010113 (callsite+1) `JPL I -50` -> ptr 010043 = routine **007500** | PROVEN |
| Success path | 010114 (callsite+2) `JMP I -43` -> ptr 010051 = routine **010613** (command loop) | PROVEN |

The call is one instruction in a short sequence (`010063`, `010076`, `010104`,
`010112`) that each loads the F-image name descriptor and issues a different
subfunction; only `010112` (`JPL I 151`) resolves to the SPRNM thunk.

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| SPRNM | 074B / 0x3C / 60 | 146602 | `,X 6` = name descriptor (F register, 3 words) from F-image `B-113` [010107 `LDF ,B -113`; 010111 `STF ,X 6`] | err=010113 -> 007500; ok=010114 -> 010613 |

Thunk bytes (verified): `146602`=`170474` (`SAA 74`), `146603`=`125001`, `146604`=`146244`.

## What it does

1. Loads the process-name descriptor from the F-image at `B-113`. (`010107`)
2. Stores it (F register = 3 words) into gateway param slot 1. (`010110-010111`)
3. Issues `MON 60` SPRNM. (`010112`)
4. On error -> routine `007500`; on success -> command loop `010613`.

## Unknown / inferred

- **PROVEN**: the single parameter is the F register (3 words), the standard
  file-name/process-name descriptor form used throughout this program (`LDF`/`STF`).
- **INFERRED**: the descriptor at `B-113` is the operator-supplied process name.
  Its byte content was not traced; the `B-113` F-image location is PROVEN.
- **INFERRED (role)**: routine `007500` is an error/return helper and `010613` the
  interpreter command loop. Their entry addresses and that the error/success paths
  reach them are PROVEN; internal behaviour was not carved.
