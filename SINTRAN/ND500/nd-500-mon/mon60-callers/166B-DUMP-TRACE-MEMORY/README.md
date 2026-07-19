# 166B DUMP TRACE-MEMORY

MON 60 subfunction **166B** (octal) = **0x76 = 118 dec**.

Authoritative purpose (`SUBFUNCTION-TABLE.md` / `5P-P2-MON60.NPL`): **DUMP-TRACE-MEMORY**.
Server handler: `5NOPAR (generic)`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `147022` = `SAA 166 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **140654** | 140575 | 000014 | 140655 -> 140601 (inner error handler) | 140656 (LDA I 62 ...) continues the sequence |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `147022` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 140654** (in 140575):

- param1 (,X 6) = &(B-162); 140642 LDD 105 / 140643 STD ,B-162 / 140645 AAA -162 / 140646 STA ,X 6.
- param2 (,X 7) = 3-word float; 140650 LDA 101 / 140651 SWAP CLD SA DD / 140652 LDT 100 / 140653 STF ,X 7.

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Inside routine 140575's chained sequence. Passes a pointer (&B-162) and a 3-word float value. Reached as the success of the preceding 63->140755 call at 140672.

Field semantics (trace address / length) INFERRED from the DUMP-TRACE-MEMORY purpose.

## Unknown / inferred

- INFERRED: param1 = trace-memory address/descriptor, param2 = a 3-word (float-image) value; exact layout not read from server source.

## Number bases

`166B` octal = `0x76 = 118 dec`.
