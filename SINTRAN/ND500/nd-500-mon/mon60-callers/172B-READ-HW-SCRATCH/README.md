# 172B READ HW SCRATCH REGISTER FILE

MON 60 subfunction **172B** (octal) = **0x7A = 122 dec**.

Authoritative purpose (`SUBFUNCTION-TABLE.md` / `5P-P2-MON60.NPL`): **READ HW SCRATCH REGISTER FILE**.
Server handler: `5NOPAR (generic)`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `147041` = `SAA 172 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **056700** | 056042 | 000050 | 056701 JPL -74 -> 056605 | 056702 JMP 23 -> 056725 |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `147041` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 056700** (in 056042):

- param1 (,X 6) = &(B-132) (056622/056623 AAA -132 / 056625 STA ,X 6).
- param2 (,X 7) = &(B-130) (056643/056644 AAA -130 / 056646 STA ,X 7).
- param3 (,X 10) = 3-word float built from B-143 (056653 STF ,B-143 / 056654 LDF ,B-143 / 056655 STF ,X 10).

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Passes two local-address pointers and a 3-word float. An intermediate helper call at 056635 (JPL I 37 -> ptr 056674=174537, a runtime helper, not a thunk) sits between the param stores; the caller grows then shrinks the stack top by 7 around it (056626-056630 / 056640-056642) so the 172B param slots refer to the same block. PROVEN stack arithmetic.

The call site is reached via 056656 JMP 22 -> 056700 (jumping over the wrapper's pointer pool 056657-056677).

## Unknown / inferred

- INFERRED: the three params are the scratch-register-file request/return buffers; exact layout not read from server source.

## Number bases

`172B` octal = `0x7A = 122 dec`.
