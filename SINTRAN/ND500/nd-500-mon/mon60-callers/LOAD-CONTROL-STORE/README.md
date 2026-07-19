# LOAD-CONTROL-STORE

MON 60 subfunction **LDCS = 37B** (octal) = **0x1F** = 31 decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **LOAD CONTROL STORE (LOAD A FILE INTO CS)**,
server handler `ICSLOAD`.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331 = 217 locals) | PROVEN (ENTER prologue at 002662, verified via the 439-entry ENTER map) |
| This command's case | **006064 .. 006116** | PROVEN (bounded by command-loop jumps) |
| MON 60 call site | **006114** `JPL I 32` -> ptr 006146, `bank1[006146]=146445` = thunk `SAA 37` | PROVEN |
| Error path | 006115 (callsite+1) -> ptr 006130 = routine **002673** | PROVEN |
| Success path | 006116 (callsite+2) -> ptr 005731 = routine **010613** (command loop) | PROVEN |

The command is one *case* within the monitor's single large command interpreter
(routine 002662); it is not a separate ENTER routine.

## MON 60 subfunction(s) used

| Subfn | Octal / Hex | Thunk | Params (yaml) | Parameter-block layout (this handler) | Skip/Error |
|---|---|---|---|---|---|
| LDCS | 37B / 0x1F | 146445 | `<CS addr> <no of words> <file name>` | `,X 6` = &csaddr (@B+105); `,X 7` = &count (@B+107); `,X 10` = filename (F, 3 words, @B-113) | err=006115->002673; ok=006116->010613 |

Byte citations: value at `006146` = `146445` (thunk); thunk `146445`=`SAA 37`,
`146446`=`JMP I 1`, `146447`=`146244` (gateway). Parameter stores at `006073`
(`STA ,X 6`), `006111` (`STA ,X 7`), `006113` (`STF ,X 10`).

## What it does

1. Evaluates two command-line numeric operands via helper routine `002003`
   (`SAA 1`/`SAA 2` selector; returns a 32-bit value in D) -> the control-store
   address and the word count. (`006064-006067`, `006077-006102`)
2. Loads the file-name descriptor from the F-image at `B-113`. (`006112-006113`)
3. Places `&csaddr`, `&count`, `filename` into gateway param slots 1/2/3 and
   issues `MON 60` LDCS. (`006114`)
4. On error -> internal error reporter `002673`; on success -> command loop `010613`.

**No file I/O happens on the ND-100 side.** The only MON call in the whole case is
the LDCS `MON 60`. The file is opened and read into control store by the SINTRAN
server handler `ICSLOAD` (which "just copies the file name into the MON 60 buffer,
then forwards to the ND-500", per `5P-P2-MON60.NPL`). This contradicts the task's
initial hypothesis that this handler would issue OPEN(50B)/RFILE(117B)/ALTON(33B);
those calls are not present in `006064-006116` (PROVEN by inspection).

## How it fits ND-500 init

LDCS is the writable-control-store (microcode) loader. Because the shared MON 60
gateway (146244) auto-retries on status `ECSLOAD` (002032B) and `004017B`, any other
MON 60 can *trigger* a control-store load, but LDCS is the explicit operator path to
place a microcode file into the ND-500 control store before the processor is usable.

## Unknown / inferred

- **INFERRED**: helper `002003` is the command-line numeric-argument evaluator. Its
  ENTER prologue is at `002003` (`RADD AD1 CLD SL DX`), and the `SAA n` before each
  call selects operand n; the exact parsing was not traced.
- **INFERRED**: local `B+105` = CS address, `B+107` = word count (mapping to the yaml
  parameter order). The addresses are proven; the *semantic* label rests on the yaml
  signature order.
- **INFERRED (role)**: routine `002673` is the interpreter's error reporter and
  `010613` its per-command continue/loop point. Their entry addresses and that the
  error/success paths reach them are PROVEN; their internal behaviour was not carved.
- **PROVEN**: file name occupies 3 words (F register, `LDF`/`STF`).
