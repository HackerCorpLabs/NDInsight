# 156B READ SYSTEM INFO

MON 60 subfunction **156B** (octal) = **0x6E = 110 dec**.

Authoritative purpose (`SUBFUNCTION-TABLE.md` / `5P-P2-MON60.NPL`): **READ SYSTEM INFO**.
Server handler: `5NOPAR (generic)`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `146772` = `SAA 156 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **143074** | 143066 | 000000 | 143075 -> ptr 143102 = 177327 (LEAVE error) | 143076 -> ptr 143103 = 177335 (LEAVE-SKIP) |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `146772` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 143074** (in 143066):

- param1 (,X 6): 143071 LDA 7 loads one word; 143073 STA ,X 6. Source of 'LDA 7' (word 044007) not resolved to a named object.

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Tiny wrapper (0 locals): ENTER, load one word, issue the subfunction, LEAVE. 143077-143103 is its pointer pool (177300, 146772, 177327, 177335).

## Unknown / inferred

- INFERRED: the single word passed in ,X 6 is the system-info request/buffer; the 'LDA 7' source was not resolved.

## Number bases

`156B` octal = `0x6E = 110 dec`.
