# 055B-SPLAC

MON 60 subfunction **SPLAC = 55B = 0x2D = 45 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **START-PLACE**, server handler `ISPLACE`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

60B yaml client name = `SPLAC`. **Cross-reference:** both SPLAC call sites are
reached from the domain-placement routines that `../START-STANDARD-DOMAIN/`
documents. At each site SPLAC (`55B`) is the ELSE arm of a 2-way branch whose
IF arm calls SRESPL (`140B`, thunk 146737); the selector is a local flag
(`B-163` at site 1, `B-165` at site 2). No `,X` parameter is stored immediately
before either SPLAC call.

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146530` | `170455` | `SAA 55` (subfunction 55B) |
| `146531` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146532` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **043552** `JPL I 124` -> ptr `043676`, `bank1[043676]=146530` = thunk `SAA 55` | PROVEN |
| Error path (callsite+1) | 043553 `JPL I 122` -> routine **043321** (error path) | PROVEN |
| Success path (callsite+2) | 043554 (fall through) `STZ ,B -104` | PROVEN |
| Enclosing ENTER routine | 043011 (framesize 000717) - a standalone ENTER routine | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |
| MON 60 call site | **063065** `JPL I 65` -> ptr `063152`, `bank1[063152]=146530` = thunk `SAA 55` | PROVEN |
| Error path (callsite+1) | 063066 `JPL I 52` -> routine **062446** (error path) | PROVEN |
| Success path (callsite+2) | 063067 (fall through) `STZ ,B -103` | PROVEN |
| Enclosing ENTER routine | 062257 (framesize 000544) - a standalone ENTER routine | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| (none) | -- | no `,X` parameter store immediately precedes this call site |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. START-PLACE is the opening bracket of a domain PLACE sequence (with `006B` LOAD-ONE-SEGMENT and `056B` END-PLACE).
2. At each site SPLAC is selected only when a local flag is zero; otherwise the paired SRESPL (`140B`, thunk 146737 = `SAA 40`) is issued instead.
3. No per-call parameter is written into the gateway block immediately before either SPLAC call (PROVEN by inspection): the subfunction is issued with the parameter block as previously set up (i.e. START-PLACE carries no fresh operand here).
4. On error -> `043321` (site 1) / `062446` (site 2); on success -> fall through to the next placement step.

## Unknown / inferred

- **PROVEN**: no `,X 6/7/10` store precedes either SPLAC call; the same is true of the paired SRESPL calls at `043547`/`063062`.
- **PROVEN**: the 2-way branch (SPLAC vs SRESPL 140B) keyed on `B-163`/`B-165`.
- **INFERRED (role)**: `043321`/`062446` are the enclosing routines' error handlers (targets PROVEN, internals not carved).
- **INFERRED**: START-PLACE / `ISPLACE` semantics from `SUBFUNCTION-TABLE.md`; thunk `SAA 55` PROVEN. See `../START-STANDARD-DOMAIN/` for the enclosing placement logic.

octal=hex=decimal: 55B = 0x2D = 45 decimal
