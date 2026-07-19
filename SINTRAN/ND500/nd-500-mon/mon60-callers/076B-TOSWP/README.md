# TOSWP

MON 60 subfunction **TOSWP = 076B** (octal) = **0x3E** = **62** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **MESSAGE TO SWAPPER**, server
handler `ITOSWP`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Named by subfunction (`TOSWP`). No operator command name is invented here.

## Handler location - five call sites

Every site marshals exactly one parameter: a pointer to a message block in the
caller's frame (`RADD ,B -> AAA <offset> -> LDX ,B -176 -> STA ,X 6`).

| Call site | JPL | ptr -> thunk | Enclosing routine | Param `,X 6` | Error (callsite+1) | Success (callsite+2) | Status |
|---|---|---|---|---|---|---|---|
| **073362** | `JPL I 22` | 073404 -> 146610 | **073115** (fs 000336) | &(B-167) [073361] | 073363 -> 177327 | 073364 -> 177335 | PROVEN |
| **073675** | `JPL I 14` | 073711 -> 146610 | 073535 (fs 000030) | &(B-160) [073674] | 073676 -> 177327 | 073677 -> 177335 | PROVEN |
| **073741** | `JPL I 7`  | 073750 -> 146610 | 073713 (fs 000016) | &(B-166) [073740] | 073742 -> 177327 | 073743 -> 177335 | PROVEN |
| **074003** | `JPL I 6`  | 074011 -> 146610 | 073752 (fs 000025) | &(B-160) [074002] | 074004 -> 177327 | 074005 -> 177335 | PROVEN |
| **107434** | `JPL I 43` | 107477 -> 146610 | 103722 (fs 000605) | &(B-165) [107433] | 107435 `JMP I ,B -135` | 107436 `SAA 20` | PROVEN |

**Cross-reference**: call site `073362` lies inside routine **073115**, the
LIST / SET-SYSTEM-PARAMETERS handler already carved under
`SINTRAN/ND500/nd-500-mon/mon60-callers/LIST-SYSTEM-PARAMETERS/`
(that routine also issues RSYSP 073132, RDSWP 073152, and WSYSP 073354).

Thunk bytes (verified): `146610`=`170476` (`SAA 76`), `146611`=`125001`, `146612`=`146244`.
The four `073xxx`/`073xxx` sites take the standard subroutine returns (177327 =
LEAVE-value error, 177335 = LEAVE-SKIP success); site `107434` returns error through
a frame-relative `JMP I ,B -135`.

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| TOSWP | 076B / 0x3E / 62 | 146610 | `,X 6` = pointer to a message block in the caller's frame (one param at all 5 sites) | see per-site table |

## What it does

At each site the caller builds a pointer to a message block already assembled in its
local frame, stores it in gateway param slot 1, and issues `MON 60` TOSWP to hand
that message to the ND-500 swapper. Success/error take the standard returns.

## Unknown / inferred

- **PROVEN**: exactly one parameter (`,X 6`) is stored at every site; no `,X 7`/`,X 10`
  store precedes any TOSWP call.
- **INFERRED**: the pointed-to word block is a swapper message; its layout/opcode was
  not traced. The store operations and the frame offsets are PROVEN.
- **INFERRED (roles)**: routines 073535 / 073713 / 073752 are small standalone
  helpers that each send one swapper message; 103722 is a larger routine. Their ENTER
  addresses and framesizes are PROVEN; internal behaviour was not carved. `177327`
  (LEAVE-value) and `177335` (LEAVE-SKIP) are the runtime return helpers (PROVEN).
