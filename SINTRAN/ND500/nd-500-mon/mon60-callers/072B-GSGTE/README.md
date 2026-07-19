# GSGTE

MON 60 subfunction **GSGTE = 072B** (octal) = **0x3A** = **58** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **READ A PHYS.SEGMENT TABLE ENTRY
FROM SYS.MON**, server handler `5NOPAR` (generic input path; the message is
forwarded to the ND-500 without special parameter marshalling).

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

Named by subfunction (`GSGTE`, the yaml client name). No operator command name is
invented here.

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **110365** (framesize 002250) | PROVEN (ENTER prologue at 110365; 439-routine map) |
| MON 60 call site | **110447** `JPL I 122` -> ptr 110571, `bank1[110571]=146574` = thunk `SAA 72` | PROVEN |
| Error path | 110450 (callsite+1) `JPL -16` (relative) | PROVEN target polarity; branch destination is a relative JPL |
| Success path | 110451 (callsite+2) `SAA 1` (falls through into the routine's continuation) | PROVEN |

This is one call site inside a large routine (110365); it is not a standalone
per-command routine.

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout (this call site) | Skip/Error |
|---|---|---|---|---|
| GSGTE | 072B / 0x3A / 58 | 146574 | `,X 6` = &local(B+34) [110440-110443]; `,X 7` = &local(B-42) [110444-110446] | err=110450 (callsite+1); ok=110451 (callsite+2) |

Thunk bytes (verified): `146574`=`170472` (`SAA 72`), `146575`=`125001` (`JMP I 1`),
`146576`=`146244` (gateway). Parameter stores: `110443` (`STA ,X 6`), `110446`
(`STA ,X 7`).

## What it does

1. Computes `A := B+34` and stores `&(B+34)` into gateway param slot 1. (`110440-110443`)
2. Computes `A := B-42` and stores `&(B-42)` into gateway param slot 2. (`110444-110446`)
3. Issues `MON 60` GSGTE. (`110447`)
4. On error -> callsite+1 (`110450`); on success -> callsite+2 (`110451`).

GSGTE reads one physical-segment table entry from the system monitor's tables.

## Unknown / inferred

- **INFERRED**: of the two parameter pointers, one supplies the physical-segment
  index (input) and one is the buffer that receives the table entry (output). The
  store addresses `,X 6` = &(B+34) and `,X 7` = &(B-42) are PROVEN; the input/output
  role split rests on the subfunction's stated purpose (READ ... ENTRY), not on a
  traced data flow. GSGTE is the physical-segment twin of GPRTE (070B, READ A
  PROCESS TABLE ENTRY); both dispatch to `5NOPAR`.
- **PROVEN**: exactly two parameter words are stored (`,X 6`, `,X 7`); no `,X 10`
  store appears in `110432-110452`.
- **INFERRED (role)**: enclosing routine 110365 is a larger service routine; its
  full behaviour was not carved. Its ENTER prologue address and framesize are PROVEN.
