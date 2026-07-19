# 173B SET CPU STATUS

MON 60 subfunction **173B** (octal) = **0x7B = 123 dec**.

Authoritative purpose (`SUBFUNCTION-TABLE.md` / `5P-P2-MON60.NPL`): **SET CPU STATUS (ICPUSTAT)**.
Server handler: `ICPUSTAT`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `147044` = `SAA 173 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **032605** | 032442 | 000046 | 032606 -> ptr 032612 = 177327 (LEAVE error) | 032607 -> ptr 032616 = 177335 (LEAVE-SKIP) |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `147044` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 032605** (in 032442):

- param1 (,X 6) = word at (B-172)+3 (032567 LDX ,B-172 / 032570 AAX 3 / 032571 LDA ,X 0 / 032573 STA ,X 6).
- param2 (,X 7) = &(B-137) (032575 AAA -137 / 032576 STA ,X 7).
- param3 (,X 10) = &(B-141) (032600 AAA -141 / 032601 STA ,X 10).
- param4 (,X 11) = &(B-135) (032603 AAA -135 / 032604 STA ,X 11).

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Passes one scalar (fetched from an array element at (B-172)+3) plus three local-address pointers. On SET-CPU-STATUS the scalar is the status word and the pointers are return/scratch buffers. Field semantics INFERRED.

032610-032616 includes this routine's pointer pool (177300, 177327, 147044 thunk, 177335).

## Unknown / inferred

- INFERRED: param1 = CPU status word, params 2-4 = return/scratch buffers; not read from server source.

## Number bases

`173B` octal = `0x7B = 123 dec`.
