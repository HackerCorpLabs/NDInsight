# 150B LIST TIME-QUEUE

MON 60 subfunction **150B** (octal) = **0x68 = 104 dec**.

Authoritative purpose (`SUBFUNCTION-TABLE.md` / `5P-P2-MON60.NPL`): **LIST ND-500 TIME-QUEUE (ILI5TQU)**.
Server handler: `ILI5TQU`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `146753` = `SAA 150 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **111614** | 111604 | 000004 | 111615 -> ptr 111725 = 177327 (LEAVE with error) | 111616 (SAA 41 ...) falls through, continues in routine |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `146753` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 111614** (in 111604):

- param1 (,X 6): 111607 LDA ,B-172 (routine incoming local); 111610 STA I 112 / 111611 LDA 112 (indirection through pointer word at 112); 111613 STA ,X 6.

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Standalone small routine (4 locals). Passes a single parameter derived from its own incoming argument (B-172), indirected through the pointer word at location 112.

## Unknown / inferred

- INFERRED: parameter is the time-queue selector/buffer; the indirection through 112 was not fully traced.

## Number bases

`150B` octal = `0x68 = 104 dec`.
