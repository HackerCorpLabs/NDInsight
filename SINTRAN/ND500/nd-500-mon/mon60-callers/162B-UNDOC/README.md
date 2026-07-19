# 162B UNDOC

MON 60 subfunction **162B** (octal) = **0x72 = 114 dec**.

**Undocumented (server dispatch 5NOPAR).** There is no `FUNCTION=` comment for code
`162` in `5P-P2-MON60.NPL`; the SINTRAN dispatch for it is the generic `5NOPAR` path.
The subfunction *purpose* cannot be recovered from this ND-100 binary alone; only what
the **caller** does is carved here.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `147006` = `SAA 162 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **141266** | 140763 | 000032 | 141267 -> ptr 141302 = 140767 (inner error handler) | 141270 -> ptr 141304 = 177335 (LEAVE-SKIP) |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `147006` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 141266** (in 140763):

- FOUR pointer params: 141254 STA ,X 6 = &(B-154); 141257 STA ,X 7 = &(B-152); 141262 STA ,X 10 = &(B-150); 141265 STA ,X 11 = &(B-146). PROVEN.

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Passes four consecutive local addresses (&B-154, &B-152, &B-150, &B-146) as MON60 params. The four-address shape resembles a read/return-buffer call, but the purpose is undocumented.

## Unknown / inferred

- UNKNOWN: the subfunction purpose and the meaning of each of the four buffers (no FUNCTION= comment; generic 5NOPAR).

## Number bases

`162B` octal = `0x72 = 114 dec`.
