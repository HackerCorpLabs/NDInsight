# 056B-EPLAC

MON 60 subfunction **EPLAC = 56B = 0x2E = 46 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **END-PLACE**, server handler `IEPLACE`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

60B yaml client name = `EPLAC`. **Cross-reference:** both EPLAC call sites are
the closing bracket of the same domain-placement routines carved in
`../START-STANDARD-DOMAIN/` (paired with 055B SPLAC).

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146533` | `170456` | `SAA 56` (subfunction 56B) |
| `146534` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146535` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **044062** `JPL I 35` -> ptr `044117`, `bank1[044117]=146533` = thunk `SAA 56` | PROVEN |
| Error path (callsite+1) | 044063 `JPL I -166` -> routine **043321** (error path) | PROVEN |
| Success path (callsite+2) | 044064 `JMP 10` -> 044074 (next step) | PROVEN |
| Enclosing ENTER routine | 043011 (framesize 000717) - a standalone ENTER routine | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |
| MON 60 call site | **063342** `JPL I 23` -> ptr `063365`, `bank1[063365]=146533` = thunk `SAA 56` | PROVEN |
| Error path (callsite+1) | 063343 `JPL I 56` -> routine **062446** (error path) | PROVEN |
| Success path (callsite+2) | 063344 (fall through) `LDA ,B -100` | PROVEN |
| Enclosing ENTER routine | 062257 (framesize 000544) - a standalone ENTER routine | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| `,X 6` (+7,+10) @044061/@063341 | `STF ,X 6` | parameter 1 = F-register descriptor (3 words) |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. END-PLACE closes the PLACE sequence opened by SPLAC (`055B`).
2. At each site the F register (3 words) is stored into gateway slots 6/7/10 as parameter 1, then `MON 60` EPLAC is issued.
3. On error -> `043321` (site 1) / `062446` (site 2); on success -> the next step / fall through.

## Unknown / inferred

- **PROVEN**: param 1 = F-register descriptor (`STF ,X 6`, 3 words) at both sites.
- **UNKNOWN**: the precise contents of that F descriptor at EPLAC time (it is loaded from the frame; the field layout was not carved).
- **INFERRED (role)**: `043321`/`062446` are the routines' error handlers.
- **INFERRED**: END-PLACE / `IEPLACE` semantics from `SUBFUNCTION-TABLE.md`; thunk `SAA 56` PROVEN. See `../START-STANDARD-DOMAIN/`.

octal=hex=decimal: 56B = 0x2E = 46 decimal
