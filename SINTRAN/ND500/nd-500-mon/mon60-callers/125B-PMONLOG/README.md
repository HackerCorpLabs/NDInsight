# PMONLOG

MON 60 subfunction **PMONLOG = 125B** (octal) = **0x55** = **85** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**READ MONCALL LOG DATA (PRINT MONCALL LOG)**, server handler `IPRIMLOG`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **111217** (framesize 001411 = 777 dec) | PROVEN (111217 `RADD AD1 CLD SL DX`; 111220 `JPL I 166` -> ptr `111406`=`177300` ENTER) |
| MON 60 call site | **111232** `JPL I 156` -> ptr `111410`, `bank1[111410]=146701` = thunk `SAA 125` | PROVEN |
| Error path | 111233 (callsite+1) -> ptr `111411` = **177327** LEAVE(value) | PROVEN |
| Success path | 111234 (callsite+2) - continues in-line | PROVEN |

Routine 111217 is also reached from the command interpreter case at `007315`
(`JPL I 70` -> ptr `007405` = `111217`) - i.e. the PRINT-MONCALL-LOG command
invokes this routine, which then issues the PMONLOG MON 60 here.

## Thunk verification (PROVEN, read from bytes)

`146701 170525 SAA 125` ; `146702 125001 JMP I 1` ; `146703 146244` (gateway).

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| PMONLOG | 125B / 0x55 / 85 | 146701 | `,X 6` = a 3-word `F` descriptor assembled from `&local(B-163)` (in T) and a pooled word @111407 (in D) | err=111233->177327; ok=111234 in-line |

Only slot 6 is stored (`111231 STF ,X 6`, 3 words).

## What it does

1. Assembles a 3-word `F` descriptor: `T := &local(B-163)`, `D :=` pooled word at
   P-relative address `111407` (via `LDA 162` + `SWAP`). (`111222-111227`)
2. Places `F` in gateway param slot 1 and issues `MON 60` PMONLOG. (`111230-111232`)
3. On error -> `177327` LEAVE(value); on success -> continues in-line.

## Unknown / inferred

- **PROVEN**: subfunction identity, thunk bytes, the single 3-word `F` param store,
  and the error/success targets.
- **INFERRED**: the descriptor holds {buffer pointer, length/count} for the
  moncall-log read; exact field roles not traced. Server handler `IPRIMLOG`
  reads/prints the monitor-call log.
