# 060B-LIMEM

MON 60 subfunction **LIMEM = 60B = 0x30 = 48 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **LIST MEMORY CONFIGURATION**, server handler `5NOPAR`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

60B yaml client name = `LIMEM`. This is the MEMORY-CONFIGURATION command
(INDEX.md 2.1).

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146541` | `170460` | `SAA 60` (subfunction 60B) |
| `146542` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146543` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **135532** `JPL I 144` -> ptr `135676`, `bank1[135676]=146541` = thunk `SAA 60` | PROVEN |
| Error path (callsite+1) | 135533 `JPL I 144` -> routine **177327** = LEAVE-with-value (propagates MON 60 error code A to caller) | PROVEN |
| Success path (callsite+2) | 135534 (fall through) `RADD CLD SB DA` | PROVEN |
| Enclosing ENTER routine | 135502 (framesize 000060) - a standalone ENTER routine | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| `,X 6` | `135531 STA ,X 6` | parameter 1 = `&(B-164)` = buffer to receive the memory-configuration list |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. Sets `B-127 := -1` (a sentinel), then passes `&(B-164)` as the receive buffer. (`135524-135531`)
2. Issues `MON 60` LIMEM to list the ND-500 memory configuration. (`135532`)
3. On error LEAVEs with the error code (`177327`); on success falls through to process the returned list.

## Unknown / inferred

- **PROVEN**: param 1 = `&(B-164)` (`135531`).
- **INFERRED (semantic)**: `B-164` receives a memory-configuration list; the `SAA -1`/`B-127` sentinel role was not carved.
- **PROVEN (role)**: `177327` = LEAVE-with-value (compiler runtime, prog.md sec 4.3).
- **INFERRED**: LIST MEMORY CONFIGURATION purpose from `SUBFUNCTION-TABLE.md`; thunk `SAA 60` PROVEN.

octal=hex=decimal: 60B = 0x30 = 48 decimal
