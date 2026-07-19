# 167B UNDOC

MON 60 subfunction **167B** (octal) = **0x77 = 119 dec**.

**Undocumented (server dispatch 5NOPAR).** There is no `FUNCTION=` comment for code
`167` in `5P-P2-MON60.NPL`; the SINTRAN dispatch for it is the generic `5NOPAR` path.
The subfunction *purpose* cannot be recovered from this ND-100 binary alone; only what
the **caller** does is carved here.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `147025` = `SAA 167 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **140621** | 140575 | 000014 | 140622 -> 140601 (inner error handler) | 140623 (SAA 7 ...) continues the sequence |
| **143057** | 143054 | 000000 | 143060 -> ptr 143064 = 177327 | 143061 -> ptr 143065 = 177335 |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `147025` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 140621** (in 140575):

- NONE freshly marshalled: no 'STA ,X n' between the preceding 165B call (140617) and this call. Generic dispatch. PROVEN.

**Call site 143057** (in 143054):

- NONE. Parameterless wrapper. PROVEN.

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Two call sites. Site 140621 is the success (callsite+2) target of the 165B call at 140617 - a link in routine 140575's chained sequence, with no fresh parameter store. Site 143057 is a standalone parameterless wrapper (pool 143062-143065).

## Unknown / inferred

- UNKNOWN: the subfunction purpose (no FUNCTION= comment; generic 5NOPAR).

## Number bases

`167B` octal = `0x77 = 119 dec`.
