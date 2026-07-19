# DEFINF

MON 60 subfunction **DEFINF = 142B** (octal) = **0x62** = **98** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**(redefine default infant file)** (dispatch `5NOPAR`; no verbatim `FUNCTION=` prose).

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk verification (PROVEN, read from bytes)

`146745 170542 SAA 142` ; `146746 125001 JMP I 1` ; `146747 146244` (gateway).

## Call sites (6) and enclosing routines

**Group A** - inside command interpreter ENTER-routine **002662** (framesize
000331); pointer word `003072`=146745. The block `002715-002724` is a local
error/retry handler that also issues DEFINF.

| Call site | Instr | param1 source (`,X 6`) | Error (callsite+1) | Success (callsite+2) |
|---|---|---|---|---|
| **002723** | `JPL I 147` | pooled word @003071 (`002720 LDA 151`) | 002724 -> ptr `003073`=**177327** | 002725 in-line |
| **003014** | `JPL I 56` | pooled word @003106 (`003011 LDA 75`) | 003015 -> **002715** (local) | 003016 in-line |
| **003034** | `JPL I 36` | pooled word @003071 (`003031 LDA 40`) | 003035 -> **002715** (local) | 003036 in-line |

**Group B** - inside ENTER-routine **030302** (framesize 000004; 030302 `RADD AD1
CLD SL DX`, 030303 `JPL I 77` -> ptr `030402`=`177300` ENTER); pointer word
`030404`=146745. The block `030306-030310` is a local error/retry handler.

| Call site | Instr | param1 source (`,X 6`) | Error (callsite+1) | Success (callsite+2) |
|---|---|---|---|---|
| **030314** | `JPL I 70` | pooled word @030403 (`030311 LDA 72`) | 030315 -> ptr `030405`=**177327** | 030316 in-line |
| **030370** | `JPL I 14` | pooled word @030417 (`030365 LDA 32`) | 030371 -> **030306** (local) | 030372 -> ptr `030420` |
| **030377** | `JPL I 5`  | pooled word @030403 (`030374 LDA 7`)  | 030400 -> **030306** (local) | 030401 -> ptr `030421` |

(Pooled-word EAs are P-relative: EA = instruction address + displacement.)

## MON 60 parameter block (PROVEN)

Every one of the six sites stores exactly **one** parameter, slot 6 = a pooled
word loaded P-relative. No slots 7/10 are stored at any site.

## What it does

Each caller loads a pooled constant into gateway param slot 1 and issues
`MON 60` DEFINF to redefine the default infant file. The six variants differ only
in which pooled constant they pass, consistent with several command contexts
setting different default-infant-file parameters through the same primitive.

## Unknown / inferred

- **PROVEN**: subfunction identity (142B), thunk bytes, that all six sites
  marshal exactly one param (slot 6) from a pooled P-relative word, and the
  error/success targets.
- **INFERRED**: the pooled words are default-infant-file name/parameter constants;
  their contents were not traced. Handler `5NOPAR` = generic forward path on the
  SINTRAN side.
