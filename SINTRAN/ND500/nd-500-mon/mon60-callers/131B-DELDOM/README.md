# DELDOM (DELETE STANDARD DOMAIN)

MON 60 subfunction **DELDOM = 131B** (octal) = **0x59** = **89** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**DELETE STANDARD DOMAIN**, server handler `IDLSYDOM`.

Standard-domain family: 127B `DEFDOM` (define, see `127B-DEFDOM`), 130B start
(already carved as `START-STANDARD-DOMAIN`), 131B `DELDOM` (delete).

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331) - the command interpreter | PROVEN |
| MON 60 call site | **007347** `JPL I 42` -> ptr `007411`, `bank1[007411]=146715` = thunk `SAA 131` | PROVEN |
| Error path | 007350 (callsite+1) -> ptr `007171` = routine **002673** (interpreter error reporter) | PROVEN |
| Success path | 007351 (callsite+2) -> ptr `007376` = **010613** (command loop) | PROVEN |

## Thunk verification (PROVEN, read from bytes)

`146715 170531 SAA 131` ; `146716 125001 JMP I 1` ; `146717 146244` (gateway).

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| DELDOM | 131B / 0x59 / 89 | 146715 | `,X 6` = the 3-word `F` descriptor from `B-113` = the standard-domain name | err=007350->002673; ok=007351->010613 |

Only slot 6 is stored (`007346 STF ,X 6`, 3 words).

## What it does

1. Loads the domain-name descriptor (`F` register, 3 words) from `B-113`. (`007344`)
2. Places it in gateway param slot 1 and issues `MON 60` DELDOM. (`007345-007347`)
3. On error -> interpreter error reporter `002673`; on success -> command loop `010613`.

## Unknown / inferred

- **PROVEN**: subfunction identity, thunk bytes, the single 3-word param store,
  and the error/success targets.
- **PROVEN**: the parameter is a 3-word `F` descriptor (LDF/STF) = the standard-
  domain name. Server handler `IDLSYDOM` deletes the standard domain.
