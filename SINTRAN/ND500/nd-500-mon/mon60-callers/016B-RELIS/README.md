# 016B-RELIS

MON 60 subfunction **RELIS = 16B** (octal) = **0x0E** = **14** decimal.
NPL purpose (authoritative, `SUBFUNCTION-TABLE.md`): **RELEASE ND-500 PROCESS**.
Server handler: **5NOPAR** (generic forward path).

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

| Addr | Word | Meaning |
|---|---|---|
| 146362 | 170416 | `SAA 16` -> subfunction code 016 (14 dec) |
| 146363 | 125001 | `JMP I 1` |
| 146364 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 001126 | ENTER-routine **001072** (framesize 000001), standalone | `JPL I 17` -> 001145 | `bank1[001145]=146362` | `SAA 16` | PROVEN |
| 010322 | **CASE inside command interpreter 002662** (framesize 000331, spans 002662-010634) | `JPL I 172` -> 010514 | `bank1[010514]=146362` | `SAA 16` | PROVEN |

## Parameter block (X := b.-176)

**Neither site stores a MON 60 parameter into the block.**

- 001126: the `LDX ,B -176`/`STA ,X 6` at 001120-001121 belong to the *preceding* call
  at 001122 (routine 171557), not to RELIS. RELIS is issued as the success continuation
  of the 001124 call with no block parameter of its own.
- 010322: 010320 `SAA 1` / 010321 `STA I -26` writes `1` to the location `[010273]`
  (a flag), NOT into the `,X` parameter block.

Consistent with the `5NOPAR` handler (which derives the process number server-side).

## Skip / error handling

- 001126: err 001127 (`JPL -31`->001076, the routine's loop head); ok 001130 (`SAA 1`).
- 010322: err 010323 (`JPL I -45`->010256; `bank1[010256]=007500` shared error routine);
  ok 010324 (falls into the next interpreter case).

## Cross-reference

RELIS (16B, release) is the counterpart of `015B-RESRV` (reserve ND-500 process).

## Unknown / inferred

- **INFERRED**: the flag written by `STA I -26` at 010321 (to `[010273]`) is a
  release-mode indicator, not a MON 60 parameter. The store target `[010273]` is PROVEN
  as P-relative; its meaning is inferred.
- **PROVEN**: thunk bytes, both pointer resolutions to 146362, both call sites, the
  absence of `,X` block stores at each, and the callsite+1/+2 targets.
