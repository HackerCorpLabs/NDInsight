# 065B-HISTN

MON 60 subfunction **HISTN = 65B = 0x35 = 53 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **READ HISTOGRAM**, server handler `IREAHIST`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

60B yaml client name = `HISTN` (PRINT-HISTOGRAM command per INDEX.md 2.2).

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146555` | `170465` | `SAA 65` (subfunction 65B) |
| `146556` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146557` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **040437** `JPL I 163` -> ptr `040622`, `bank1[040622]=146555` = thunk `SAA 65` | PROVEN |
| Error path (callsite+1) | 040440 `JPL I 163` -> routine **177327** = LEAVE-with-value (propagates error code) | PROVEN |
| Success path (callsite+2) | 040441 (fall through) `LDD I 163` | PROVEN |
| Enclosing ENTER routine | 040422 (framesize 000243) - a standalone ENTER routine | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| `,X 6` (+7,+10) | `040436 STF ,X 6` | parameter 1 = F-register descriptor (3 words) |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. Loads a descriptor into the F register (3 words) and stores it as parameter 1. (`040432-040436`)
2. Issues `MON 60` HISTN to read back histogram data. (`040437`)
3. On error LEAVEs with the error code (`177327`); on success falls through to consume the returned data.

## Unknown / inferred

- **PROVEN**: param 1 = F-register descriptor (`STF ,X 6`, 3 words).
- **UNKNOWN**: the F descriptor contents (buffer/name) at HISTN time - not carved.
- **PROVEN (role)**: `177327` = LEAVE-with-value.
- **INFERRED**: READ HISTOGRAM / `IREAHIST` from `SUBFUNCTION-TABLE.md`; thunk `SAA 65` PROVEN.

octal=hex=decimal: 65B = 0x35 = 53 decimal
