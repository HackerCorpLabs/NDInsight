# 144B CHANGE CPU

MON 60 subfunction **144B** (octal) = **0x64 = 100 dec**.

Authoritative purpose (`SUBFUNCTION-TABLE.md` / `5P-P2-MON60.NPL`): **CHANGE CPU (ICHACPU)**.
Server handler: `ICHACPU`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `146750` = `SAA 144 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **010345** | 002662 | 000331 | 010346 -> ptr 010256 = 007500 (leaf error handler, role INFERRED) | 010347 -> ptr 010513 = 010613 (command loop, PROVEN) |
| **011231** | 011043 | 000236 | 011232 -> 011114 (leaf handler, role INFERRED) | 011233 -> 011377 (continues in routine) |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `146750` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 010345** (in 002662):

- param1 (,X 6) = &(B-127), where B-127 receives the value returned in D by the numeric-arg evaluator 002003 (010336 JPL I -37 -> ptr 010277=002003). Store at 010344. Value=CPU number: INFERRED.

**Call site 011231** (in 011043):

- param1 (,X 6) = &(B-152); B-152 := D copied from B-160 (011223 LDD ,B-160 / 011224 STD ,B-152). Store at 011230. Guarded by 011221 SKP IF DA EQL ST.

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Both call sites pass a single pointer parameter in slot ,X 6 pointing at a local that holds a value. On CHANGE-CPU that value is the target CPU number (INFERRED from the ICHACPU purpose).

No file I/O or other MON call is issued around either site.

## Unknown / inferred

- INFERRED: the pointed-to value is the CPU number (the ICHACPU signature was not read from the server source).
- INFERRED (role): 007500 and 011114 are leaf error handlers; 010613 is the command loop (PROVEN elsewhere: LOAD-CONTROL-STORE).
- PROVEN: helper 002003 is the command-line numeric-arg evaluator (same helper used by LOAD-CONTROL-STORE).

## Number bases

`144B` octal = `0x64 = 100 dec`.
