# DEFDOM (DEFINE STANDARD DOMAIN)

MON 60 subfunction **DEFDOM = 127B** (octal) = **0x57** = **87** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**DEFINE STANDARD DOMAIN**, server handler `IDFSYDOM`.
Operator command **DEFINE-STANDARD-DOMAIN** (see `mon60-callers/INDEX.md` sec 2.1).

Standard-domain family: 127B `DEFDOM` (define), 130B start (already carved as
`START-STANDARD-DOMAIN`), 131B `DELDOM` (delete, see `131B-DELDOM`).

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **045463** (framesize 003116 = 1614 dec) | PROVEN (045463 `RADD AD1 CLD SL DX`; 045464 `JPL I 175` -> ptr `045661`=`177300` ENTER) |
| MON 60 call site | **046056** `JPL I 27` -> ptr `046105`, `bank1[046105]=146707` = thunk `SAA 127` | PROVEN |
| Error path | 046057 (callsite+1) -> ptr `046100` = routine **045511** | PROVEN |
| Success path | 046060 (callsite+2) -> **046070** | PROVEN |

## Thunk verification (PROVEN, read from bytes)

`146707 170527 SAA 127` ; `146710 125001 JMP I 1` ; `146711 146244` (gateway).

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| DEFDOM | 127B / 0x57 / 87 | 146707 | `,X 6` = `local(B+24)` (domain-definition operand) | err=046057->045511; ok=046060->046070 |

Only slot 6 is written adjacent to the call (`046055 STA ,X 6`).

## What it does

Routine 045463 assembles the standard-domain definition in its (large) frame,
then places `local(B+24)` in gateway param slot 1 and issues `MON 60` DEFDOM
(`046053-046056`). On error -> handler `045511`; on success -> `046070`.

## Unknown / inferred

- **PROVEN**: subfunction identity, thunk bytes, the single adjacent param store,
  and the error/success targets.
- **INFERRED**: `local(B+24)` is the domain-definition operand (name/descriptor
  or a pointer to the definition block built earlier in the routine). The exact
  contents were not traced. Server handler `IDFSYDOM` defines the standard domain.
