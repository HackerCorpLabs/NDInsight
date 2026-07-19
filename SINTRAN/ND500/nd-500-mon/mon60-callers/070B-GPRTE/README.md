# 070B-GPRTE

MON 60 subfunction **GPRTE = 70B = 0x38 = 56 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **READ A PROCESS TABLE ENTRY FROM THE SYS.MON**, server handler `5NOPAR`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

60B yaml client name = `GPRTE`. Three call sites: `073472`, `074030`, `110410`.

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146566` | `170470` | `SAA 70` (subfunction 70B) |
| `146567` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146570` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **073472** `JPL I 40` -> ptr `073532`, `bank1[073532]=146566` = thunk `SAA 70` | PROVEN |
| Error path (callsite+1) | 073473 `JPL I 32` -> routine **177327** = LEAVE-with-value (propagates error code) | PROVEN |
| Success path (callsite+2) | 073474 (fall through) `LDA ,B 30` | PROVEN |
| Enclosing ENTER routine | 073412 (framesize 000223) - a standalone ENTER routine | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |
| MON 60 call site | **074030** `JPL I 51` -> ptr `074101`, `bank1[074101]=146566` = thunk `SAA 70` | PROVEN |
| Error path (callsite+1) | 074031 `JPL I 51` -> routine **177327** = LEAVE-with-value (propagates error code) | PROVEN |
| Success path (callsite+2) | 074032 (fall through) `LDA ,B -170` | PROVEN |
| Enclosing ENTER routine | 074013 (framesize 000126) - a standalone ENTER routine | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |
| MON 60 call site | **110410** `JPL I 155` -> ptr `110565`, `bank1[110565]=146566` = thunk `SAA 70` | PROVEN |
| Error path (callsite+1) | 110411 `JPL -15` -> routine **110374** (a local error handler; direct P-relative JPL) | PROVEN (direct P-relative) |
| Success path (callsite+2) | 110412 (fall through) `STZ ,B 41` | PROVEN |
| Enclosing ENTER routine | 110365 (framesize 002250) - a standalone ENTER routine | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| `,X 6` @073466/@074024/@110404 | `STA ,X 6` | parameter 1 (site1 `&(B-166)`, site2 `&(B-172)`, site3 value from `LDA 162`) |
| `,X 7` @073471/@074027/@110407 | `STA ,X 7` | parameter 2 (site1 `&(B-75)`, site2 `&(B-170)`, site3 `&(B-166)`) |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. Reads one ND-500 process-table entry from the system monitor.
2. Each of the three sites sets two parameters (`,X 6` and `,X 7`): a selector/index and a receive-buffer pointer into the local frame.
3. Sites 1 and 2 propagate a MON 60 error via LEAVE-with-value (`177327`); site 3 branches to a local error handler `110374`. All three fall through on success.

## Unknown / inferred

- **PROVEN**: two parameters at `,X 6`/`,X 7` at each of the three sites (addresses cited above).
- **INFERRED (semantic)**: which of the two is the process index and which the receive buffer was not carved; per NPL the call reads a process-table entry.
- **PROVEN (role)**: `177327` = LEAVE-with-value (sites 1,2). `110374` (site 3) is a local error handler (target PROVEN, internals not carved).
- **INFERRED**: READ A PROCESS TABLE ENTRY FROM THE SYS.MON from `SUBFUNCTION-TABLE.md`; thunk `SAA 70` PROVEN.

octal=hex=decimal: 70B = 0x38 = 56 decimal
