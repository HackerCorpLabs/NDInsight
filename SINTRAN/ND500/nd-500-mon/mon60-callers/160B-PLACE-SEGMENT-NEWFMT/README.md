# 160B PLACE SEGMENT (NEW FORMAT)

MON 60 subfunction **160B** (octal) = **0x70 = 112 dec**.

Authoritative purpose (`SUBFUNCTION-TABLE.md` / `5P-P2-MON60.NPL`): **LOAD (PLACE) ONE SEGMENT (NEW DOMAIN FORMAT) (IN5SEGLOAD)**.
Server handler: `IN5SEGLOAD`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `147000` = `SAA 160 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **060562** | 060374 | 000004 | 060563 JMP I ,B-155 -> ptr 060406 = 032006 (frame-relative error exit) | 060564 continues |
| **061135** | 060374 | 000004 | 061136 JMP I ,B-155 -> ptr 060761 = 047060 | 061137 continues |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `147000` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 060562** (in 060374):

- param1 (,X 6) = A from 060546 LDA 64; store 060550.
- param2 (,X 7) = &(B-150); 060551/060552/060553.
- param3 (,X 10) = A from 060554 LDA 57; store 060555.
- param4 (,X 11) = 3-word float (060556 SAA 36 / 060560 LDT 54 / 060561 STF ,X 11).

**Call site 061135** (in 060374):

- param1 (,X 6) = A from 061121 LDA 142; store 061123.
- param2 (,X 7) = &(B-150); store 061126.
- param3 (,X 10) = A from 061127 LDA 135; store 061130.
- param4 (,X 11) = 3-word float; store 061134.

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Two call sites in the same routine (060374). Each passes four params: two scalar words, a local-address pointer (&B-150), and a 3-word float image - the new-domain-format segment descriptor. Field semantics INFERRED.

## Unknown / inferred

- INFERRED: the four params form a segment-load descriptor (new domain format); exact fields not read from server source.

## Number bases

`160B` octal = `0x70 = 112 dec`.
