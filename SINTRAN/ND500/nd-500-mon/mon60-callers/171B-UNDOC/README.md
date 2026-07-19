# 171B UNDOC

MON 60 subfunction **171B** (octal) = **0x79 = 121 dec**.

**Undocumented (server dispatch 5NOPAR).** There is no `FUNCTION=` comment for code
`171` in `5P-P2-MON60.NPL`; the SINTRAN dispatch for it is the generic `5NOPAR` path.
The subfunction *purpose* cannot be recovered from this ND-100 binary alone; only what
the **caller** does is carved here.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `147036` = `SAA 171 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **123671** | 123577 | 000064 | 123672 -> ptr 124021 = 177327 (LEAVE error) | 123673 (JMP 124 -> 124017) continues |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `147036` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 123671** (in 123577):

- param1 (,X 6) = &(B-156) (123662 RADD / 123663 AAA -156 / 123665 STA ,X 6).
- param2 (,X 7) = &(B-160) (123666 RADD / 123667 AAA -160 / 123670 STA ,X 7).

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Passes two local-address pointers (&B-156, &B-160). A separate non-MON60 helper call precedes at 123656 (JPL I 123 -> ptr 124001=053270, not a thunk; role not traced).

## Unknown / inferred

- UNKNOWN: the subfunction purpose (no FUNCTION= comment; generic 5NOPAR).
- UNKNOWN: the meaning of the two buffers.

## Number bases

`171B` octal = `0x79 = 121 dec`.
