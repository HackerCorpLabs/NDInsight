# 154B DEBUG SWAPPER

MON 60 subfunction **154B** (octal) = **0x6C = 108 dec**.

Authoritative purpose (`SUBFUNCTION-TABLE.md` / `5P-P2-MON60.NPL`): **DEBUG SWAPPER <ON/OFF>**.
Server handler: `5NOPAR (generic)`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `146767` = `SAA 154 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **010133** | 002662 | 000331 | 010134 -> ptr 010043 = 007500 (leaf error handler, role INFERRED) | 010135 falls through (STZ I 136, no jump) |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `146767` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 010133** (in 002662):

- param1 (,X 6) = &(B-127). 010117 JAZ selects one of two constants into B-127: 010120 LDD 145 (path A) or 010123 JPL I 144 -> ptr 010267=035034 helper then 010125 LDD 143 (path B). Store at 010132.

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Single pointer parameter in ,X 6 pointing at a local that holds an ON/OFF flag (matches the '<ON/OFF>' purpose). The two-way JAZ selection at 010117 chooses the ON vs OFF constant. Semantic INFERRED.

## Unknown / inferred

- INFERRED: the parameter is an ON/OFF flag; the two constant sources (145 / 143 P-relative words, and helper 035034) were not further decoded.
- INFERRED (role): 007500 is a leaf error handler.

## Number bases

`154B` octal = `0x6C = 108 dec`.
