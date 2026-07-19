# 165B UNDOC

MON 60 subfunction **165B** (octal) = **0x75 = 117 dec**.

**Undocumented (server dispatch 5NOPAR).** There is no `FUNCTION=` comment for code
`165` in `5P-P2-MON60.NPL`; the SINTRAN dispatch for it is the generic `5NOPAR` path.
The subfunction *purpose* cannot be recovered from this ND-100 binary alone; only what
the **caller** does is carved here.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `147017` = `SAA 165 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **140617** | 140575 | 000014 | 140620 -> 140601 (inner error handler; 140605 JMP I ,B-164 -> 140421) | 140621 = the next call (167B) - chained sequence |
| **141322** | 141317 | 000000 | 141323 -> ptr 141327 = 177327 | 141324 -> ptr 141330 = 177335 |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `147017` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 140617** (in 140575):

- Nearest preceding store: 140612 LDX ,B-176 / 140613 STA ,X 6 (param1 := A; A from 140610 SAA 100 / 140611 STA I 127). In a chained sequence over shared slots; parameter attribution is not separable. INFERRED.

**Call site 141322** (in 141317):

- NONE. Parameterless wrapper. PROVEN.

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Two call sites. Site 140617 is one link in a chained subfunction sequence inside routine 140575 (each call's success is the next call; each failure branches to 140601). Site 141322 is a standalone parameterless wrapper.

Routine 140575 also issues 167B (140621) and 166B (140654) in the same chain.

## Unknown / inferred

- UNKNOWN: the subfunction purpose (no FUNCTION= comment; generic 5NOPAR).
- INFERRED: at 140617 the shared ,X 6 slot is the only nearby parameter; exact per-call parameters cannot be separated in the chain.

## Number bases

`165B` octal = `0x75 = 117 dec`.
