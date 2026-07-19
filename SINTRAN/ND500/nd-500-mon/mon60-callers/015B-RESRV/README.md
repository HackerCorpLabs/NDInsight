# 015B-RESRV

MON 60 subfunction **RESRV = 15B** (octal) = **0x0D** = **13** decimal.
NPL purpose (authoritative, `SUBFUNCTION-TABLE.md`): **RESERVE ND-500 PROCESS**.
Server handler: **5NOPAR** (generic forward path).

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

| Addr | Word | Meaning |
|---|---|---|
| 146357 | 170415 | `SAA 15` -> subfunction code 015 (13 dec) |
| 146360 | 125001 | `JMP I 1` |
| 146361 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 010333 | **CASE inside command interpreter 002662** (framesize 000331, spans 002662-010634) | `JPL I 163` -> 010516 | `bank1[010516]=146357` | `SAA 15` | PROVEN |
| 011142 | ENTER-routine **011043** (framesize 000236), standalone | `JPL I 113` -> 011255 | `bank1[011255]=146357` | `SAA 15` | PROVEN |

## Parameter block (X := b.-176; two slots, same shape at both sites)

| Slot | 010333 | 011142 |
|---|---|---|
| `,X 6` (param1) | const from `LDA 171` @010326 | const from `LDA 121` @011135 |
| `,X 7` (param2) | F register (`T=B-114`, `D=012`) @010332 | F register (`T=B-172`, `D=012`) @011141 |

Both sites build param2 the same way: `SAA 12` -> `SWAP CLD SA DD` (D:=012) -> `LDT ,B -nnn`
-> `STF ,X 7` (a 3-word F-register value written at offsets 7/10/11). The `012` constant
is common to both.

## Skip / error handling

- 010333: err 010334 (`JPL I -56`->010256; `bank1[010256]=007500` = the OTHER shared
  interpreter error routine); ok 010335.
- 011142: err 011143 (`JPL -27`->011114, a `146147` local block in routine 011043); ok 011144.

## Cross-reference

RESRV (15B, reserve) pairs with `016B-RELIS` (release ND-500 process). Both appear as
interpreter cases in the 002662 range and also in standalone routines.

## Unknown / inferred

- **INFERRED**: `,X 6` = a mode/type/process value (pool constants `LDA 171` / `LDA 121`,
  not decoded); `,X 7` = a 3-word F-register operand whose T half is a frame local and D
  half is the constant `012`. Store addresses/operands PROVEN; semantic labels inferred.
- **PROVEN**: thunk bytes, both pointer resolutions to 146357, both call sites, both
  parameter stores at each, and the callsite+1/+2 targets (incl. 010256->007500).
