# 063B-HISTA

MON 60 subfunction **HISTA = 63B = 0x33 = 51 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **START HISTOGRAM**, server handler `ISTAHIST`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

60B yaml client name = `HISTA` (START-HISTOGRAM command). This call site sits
in the interpreter's histogram block `010432-010445` alongside HISTP (`064B`
@010435) and HIREL (`066B` @010443); those have their own folders.

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146547` | `170463` | `SAA 63` (subfunction 63B) |
| `146550` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146551` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **010440** `JPL I 64` -> ptr `010524`, `bank1[010524]=146547` = thunk `SAA 63` | PROVEN |
| Error path (callsite+1) | 010441 `JPL I -163` -> routine **007500** (internal error/abort helper) | PROVEN |
| Success path (callsite+2) | 010442 `JMP 151` -> routine **010613** (command loop) | PROVEN |
| Enclosing ENTER routine | 002662 (framesize 000331) - the command interpreter | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| (none) | -- | no `,X` parameter store immediately precedes this call site |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. Issues `MON 60` HISTA to start the previously defined histogram. (`010440`)
2. No per-call parameter is written into the gateway block immediately before the call (PROVEN by inspection): START-HISTOGRAM carries no operand here.
3. On error -> `007500`; on success -> command loop `010613`.

## Unknown / inferred

- **PROVEN**: no `,X 6/7/10` store precedes `010440`.
- **INFERRED (role)**: `007500` internal error/abort helper; `010613` command loop.
- **INFERRED**: START HISTOGRAM / `ISTAHIST` from `SUBFUNCTION-TABLE.md`; thunk `SAA 63` PROVEN.
- The window shown (`010435-010442`) includes the preceding HISTP (`064B`) call at `010435` for context; HISTP is carved in `../064B-HISTP/`.

octal=hex=decimal: 63B = 0x33 = 51 decimal
