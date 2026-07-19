# WSYSP

MON 60 subfunction **WSYSP = 104B** (octal) = **0x44** = **68** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **WRITE SYSTEM PARAMETERS**, server
handler `IWSYSP`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Named by subfunction (`WSYSP`). No operator command name is invented here.

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **073115** (framesize 000336) | PROVEN (ENTER prologue at 073115) |
| MON 60 call site | **073354** `JPL I 26` -> ptr 073402, `bank1[073402]=146627` = thunk `SAA 104` | PROVEN |
| Error path | 073355 (callsite+1) `JPL I -42` -> ptr 073313 = routine **177327** (LEAVE-value) | PROVEN |
| Success path | 073356 (callsite+2) `RADD CLD SB DA` (routine continues) | PROVEN |

**Cross-reference**: routine `073115` is the LIST / SET-SYSTEM-PARAMETERS handler
already carved under
`SINTRAN/ND500/nd-500-mon/mon60-callers/LIST-SYSTEM-PARAMETERS/`.
The INDEX maps **SET-SYSTEM-PARAMETERS** to WSYSP call site `073354` (the same site
documented here). That routine also issues RSYSP `073132`, RDSWP `073152`, and
TOSWP `073362`.

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| WSYSP | 104B / 0x44 / 68 | 146627 | `,X 6` = &(B-157) [073347 `RADD SB DA`; 073350 `AAA -167`; 073351 `AAA 10` -> B-157; 073353 `STA ,X 6`] | err=073355 -> 177327; ok=073356 |

Thunk bytes (verified): `146627`=`170504` (`SAA 104`), `146630`=`125001`, `146631`=`146244`.

## What it does

1. Computes `&(B-157)` (as `B-167 + 10`) - a pointer to the system-parameter block in
   the routine's frame. (`073347-073352`)
2. Stores it into gateway param slot 1. (`073353`)
3. Issues `MON 60` WSYSP to write the system parameters into the ND-500. (`073354`)
4. On error -> LEAVE-value (`177327`) returns an error from routine `073115`; on
   success the routine continues.

## Unknown / inferred

- **PROVEN**: single parameter `,X 6` = &(B-157).
- **INFERRED**: the block at `B-157` holds the system-parameter values to write. Its
  fields were not traced here; see the LIST-SYSTEM-PARAMETERS carve for the routine
  context. The pointer computation is PROVEN.
- **PROVEN**: `177327` is the runtime LEAVE-value helper (prog.md sec 4.3).
