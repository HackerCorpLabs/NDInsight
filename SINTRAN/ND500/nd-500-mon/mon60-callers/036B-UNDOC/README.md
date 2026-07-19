# 036B UNDOC

MON 60 subfunction **036B** (octal) = **0x1E = 30 dec**.

**Undocumented (server dispatch 5NOPAR).** There is no `FUNCTION=` comment for code
`036` in `5P-P2-MON60.NPL`; the SINTRAN dispatch for it is the generic `5NOPAR` path.
The subfunction *purpose* cannot be recovered from this ND-100 binary alone; only what
the **caller** does is carved here.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `146775` = `SAA 036 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **005733** | 002662 | 000331 | 005734 -> ptr 005702 = 002673 (interpreter error reporter) | 005735 -> 005740 (005740 JMP I -7 -> ptr 005731=010613 command loop) |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `146775` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 005733** (in 002662):

- NONE. No 'STA ,X n' appears in 005727-005732; 005731/005732 are pool/mis-decoded data (prog.md sec 9.1). Caller marshals no parameters. PROVEN.

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

A no-parameter server dispatch. The caller issues the subfunction with no marshalled parameters and returns to the command loop on success.

Three 036B thunks exist (146434 / 146442 / 146775); only 146775 has a resolvable caller.

## Unknown / inferred

- UNKNOWN: the subfunction purpose. There is no FUNCTION= comment for 036 in 5P-P2-MON60.NPL and dispatch is generic 5NOPAR. Purpose cannot be recovered from this binary alone.

## Number bases

`036B` octal = `0x1E = 30 dec`.
