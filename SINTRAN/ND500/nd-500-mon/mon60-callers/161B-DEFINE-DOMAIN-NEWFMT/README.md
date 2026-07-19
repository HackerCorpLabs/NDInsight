# 161B DEFINE DOMAIN (NEW FORMAT)

MON 60 subfunction **161B** (octal) = **0x71 = 113 dec**.

Authoritative purpose (`SUBFUNCTION-TABLE.md` / `5P-P2-MON60.NPL`): **DEFINE STANDARD DOMAIN (NEW DOMAIN FORMAT) (INDFSYDOM)**.
Server handler: `INDFSYDOM`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `147003` = `SAA 161 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **071025** | 070160 | 002172 | 071026 -> ptr 071041 = 070175 (leaf error handler) | 071027 (LDA ,B-152 ...) continues |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `147003` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 071025** (in 070160):

- param1 (,X 6) = value at B-153 (071000 LDA ,B-153 / 071001 LDX ,B-176 / 071002 STA ,X 6).
- param2 (,X 7) = 3-word float from B-172 (071003 LDF ,B-172 / 071004 STF ,X 7).

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Passes a scalar (B-153) and a 3-word float image (B-172) - the new-domain-format domain descriptor. Field semantics INFERRED.

## Unknown / inferred

- INFERRED: the two params form a domain-definition descriptor (new domain format); exact fields not read from server source.

## Number bases

`161B` octal = `0x71 = 113 dec`.
