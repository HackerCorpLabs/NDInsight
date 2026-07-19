# 155B UNDOC

MON 60 subfunction **155B** (octal) = **0x6D = 109 dec**.

**Undocumented (server dispatch 5NOPAR).** There is no `FUNCTION=` comment for code
`155` in `5P-P2-MON60.NPL`; the SINTRAN dispatch for it is the generic `5NOPAR` path.
The subfunction *purpose* cannot be recovered from this ND-100 binary alone; only what
the **caller** does is carved here.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `147030` = `SAA 155 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **005741** | 002662 | 000331 | 005742 -> ptr 005702 = 002673 (error reporter) | 005743 -> ptr 005731 = 010613 (command loop) |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `147030` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 005741** (in 002662):

- NONE. 005740 (the preceding word) is a JMP; no parameter store precedes the call. PROVEN.

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

A no-parameter server dispatch, immediately following the 036B call in the same command-block region.

5P-P2-MON60.NPL marks codes 155-167 'free for patching'.

## Unknown / inferred

- UNKNOWN: the subfunction purpose (no FUNCTION= comment; generic 5NOPAR; 'free for patching').

## Number bases

`155B` octal = `0x6D = 109 dec`.
