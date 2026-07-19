# 163B UNDOC

MON 60 subfunction **163B** (octal) = **0x73 = 115 dec**.

**Undocumented (server dispatch 5NOPAR).** There is no `FUNCTION=` comment for code
`163` in `5P-P2-MON60.NPL`; the SINTRAN dispatch for it is the generic `5NOPAR` path.
The subfunction *purpose* cannot be recovered from this ND-100 binary alone; only what
the **caller** does is carved here.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `147011` = `SAA 163 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **141002** | 140763 | 000032 | 141003 -> 140767 (inner error/leaf handler) | 141004 (SAA 1 ...) continues |
| **143045** | 143042 | 000000 | 143046 -> ptr 143052 = 177327 | 143047 -> ptr 143053 = 177335 |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `147011` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 141002** (in 140763):

- NONE. Guarded by a byte test (140775 LDT ,B-157 / 140776 LBYT / 140777 SAT 131 / 141000 SKP IF DA EQL ST); no parameter store precedes the call. PROVEN.

**Call site 143045** (in 143042):

- NONE. Parameterless wrapper. PROVEN.

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Two call sites. Site 141002 (in routine 140763) is a conditional no-parameter dispatch behind a byte comparison. Site 143045 is a standalone parameterless wrapper (pool 143050-143053).

## Unknown / inferred

- UNKNOWN: the subfunction purpose (no FUNCTION= comment; generic 5NOPAR).

## Number bases

`163B` octal = `0x73 = 115 dec`.
