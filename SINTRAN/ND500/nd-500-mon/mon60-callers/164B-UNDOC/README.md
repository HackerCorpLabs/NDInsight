# 164B UNDOC

MON 60 subfunction **164B** (octal) = **0x74 = 116 dec**.

**Undocumented (server dispatch 5NOPAR).** There is no `FUNCTION=` comment for code
`164` in `5P-P2-MON60.NPL`; the SINTRAN dispatch for it is the generic `5NOPAR` path.
The subfunction *purpose* cannot be recovered from this ND-100 binary alone; only what
the **caller** does is carved here.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `147014` = `SAA 164 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **141310** | 141305 | 000000 | 141311 -> ptr 141315 = 177327 (LEAVE error) | 141312 -> ptr 141316 = 177335 (LEAVE-SKIP) |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `147014` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 141310** (in 141305):

- NONE. Parameterless wrapper; no 'STA ,X n' between ENTER and the call. PROVEN.

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Parameterless wrapper (0 locals): ENTER, issue subfunction, LEAVE. 141313-141316 is the pointer pool (177300, 147014, 177327, 177335).

## Unknown / inferred

- UNKNOWN: the subfunction purpose (no FUNCTION= comment; generic 5NOPAR).

## Number bases

`164B` octal = `0x74 = 116 dec`.
