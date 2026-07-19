# 170B READ CPU-TYPE AND MIC.VERSION

MON 60 subfunction **170B** (octal) = **0x78 = 120 dec**.

Authoritative purpose (`SUBFUNCTION-TABLE.md` / `5P-P2-MON60.NPL`): **READ ND-500 CPU-TYPE AND MIC.VERSION**.
Server handler: `5NOPAR (generic)`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Thunk `147033` = `SAA 170 ; JMP I 1 ; 146244` (gateway). Confirmed verbatim in the .asm.

## Handler location(s)

| Call site | Enclosing ENTER-routine | Framesize | Error path (callsite+1) | Success path (callsite+2) |
|---|---|---|---|---|
| **143134** | 143104 | 000004 | 143135 JPL -25 -> 143110 (inner error handler) | 143136 (SAA 1 ...) continues |

Every call site resolves as `JPL I <disp>` -> `EA=P+disp` -> `bank1[EA]` = pointer word
= thunk `147033` (PROVEN by resolving the pointer word).

## Parameter block filled before the call

**Call site 143134** (in 143104):

- param1 (,X 6) = value at 17 (143126 LDA 17 / 143127 LDX ,B-176 / 143130 STA ,X 6).
- param2 (,X 7) = a buffer address (143131 LDA 14 / 143132 AAA 2 / 143133 STA ,X 7).

The block base is `X := ,B -176` (stack top = base of the gateway frame); slot `,X n`
becomes MON 60 parameter `n-5` (PROVEN convention, prog.md sec 4.4).

## What the caller does

Passes two words: a request word and a buffer address. On READ-CPU-TYPE these are the return buffers for CPU type and microcode version. Field semantics INFERRED.

## Unknown / inferred

- INFERRED: param1/param2 are the CPU-type and mic-version request/return buffers; not read from server source.

## Number bases

`170B` octal = `0x78 = 120 dec`.
