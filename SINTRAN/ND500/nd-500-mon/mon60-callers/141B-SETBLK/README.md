# SETBLK

MON 60 subfunction **SETBLK = 141B** (octal) = **0x61** = **97** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`, via
`SINTRAN/ND500/nd-500-mon/mon60-callers/SUBFUNCTION-TABLE.md`):
**(set block size of a file)** (dispatch `5NOPAR`; no verbatim `FUNCTION=` prose).
Operator command **SET-BLOCK-SIZE** (`mon60-callers/INDEX.md` sec 2.2).

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331) - the command interpreter | PROVEN |
| This command's case | **005131 .. 005156** | PROVEN |
| MON 60 call site | **005154** `JPL I 64` -> ptr `005240`, `bank1[005240]=146742` = thunk `SAA 141` | PROVEN |
| Error path | 005155 (callsite+1) -> ptr `005017` = routine **002673** (interpreter error reporter) | PROVEN |
| Success path | 005156 (callsite+2) -> ptr `005232` = **010613** (command loop) | PROVEN |

## Thunk verification (PROVEN, read from bytes)

`146742 170541 SAA 141` ; `146743 125001 JMP I 1` ; `146744 146244` (gateway).

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| SETBLK | 141B / 0x61 / 97 | 146742 | `,X 6` = `&local(B+105)` = operand1; `,X 7` = `&local(B+107)` = operand2 | err=005155->002673; ok=005156->010613 |

Slots 6 and 7 are stored (`005135 STA ,X 6`, `005153 STA ,X 7`). Note the frame
top `B-176` is advanced +7 (`005140`) and restored -7 (`005146`) around the
nested helper call at `005142`.

## What it does

1. Stores operand1 (already in `D`) into local `B+105` and points param slot 1 at
   it. (`005131-005135`)
2. Evaluates operand2 via nested helper `005021` (selector `SAA 1`) and stores it
   into local `B+107`, pointing param slot 2 at it. (`005136-005153`)
3. Issues `MON 60` SETBLK. (`005154`)
4. On error -> interpreter error reporter `002673`; on success -> command loop `010613`.

## Unknown / inferred

- **PROVEN**: subfunction identity, thunk bytes, the two param stores, the frame-
  top adjust/restore, and the error/success targets.
- **INFERRED**: operand1/operand2 map to (file number, block size) in some order;
  the order was not traced. Helper `005021` is the command-line numeric-argument
  evaluator (same idiom as helper `002003` in LOAD-CONTROL-STORE). Handler
  `5NOPAR` = generic forward path.
