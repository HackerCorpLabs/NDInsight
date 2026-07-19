# LIST-STANDARD-DOMAINS

MON 60 subfunction **LSTDOM = 132B** (octal) = **0x5A** = 90 decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **LIST STANDARD DOMAINS**.
yaml: LSTDOM "List standard domain".

> **Naming note.** Renamed from the task's "LIST-DOMAIN" to match the authoritative
> NPL wording (132B = LIST STANDARD DOMAINS).

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Enclosing ENTER-routine | **002662** (framesize 000331) | PROVEN |
| This command's case | **007352 .. 007354** | PROVEN |
| MON 60 call site | **007352** `JPL I 40` -> ptr 007412, `bank1[007412]=146720` = thunk `SAA 132` | PROVEN |
| Error path | 007353 -> ptr 007171 = routine **002673** | PROVEN |
| Success path | 007354 -> ptr 007376 = routine **010613** (command loop) | PROVEN |

## MON 60 subfunction(s) used

| Subfn | Octal / Hex | Thunk | Params (yaml) | Parameter-block layout | Skip/Error |
|---|---|---|---|---|---|
| LSTDOM | 132B / 0x5A | 146720 | `(none)` | none | err=007353->002673; ok=007354->010613 |

Byte citations: `007412`=`146720` (thunk); thunk `146720`=`SAA 132`,
`146721`=`JMP I 1`, `146722`=`146244`.

## What it does

Issues `MON 60` LSTDOM with no parameters; the ND-500 subsystem produces the list of
standard domains (the listing is emitted by the SINTRAN side / a generic `5NOPAR`
forward). Single MON 60; no other MON calls in the case.

## Adjacent command (context)

The immediately preceding case is **DELETE STANDARD DOMAIN** (DELDOM 131B, thunk
146715) at `007347`, which passes a domain name (`,X 6`, from F @B-113). It is a
separate command case:

| Command | Subfn | Octal/Hex | Thunk | Case | MON 60 site | Params |
|---|---|---|---|---|---|---|
| DELETE-STANDARD-DOMAIN | DELDOM 131B | 0x59 | 146715 | 007344-007351 | 007347 | name (F, 3w) |
| **LIST-STANDARD-DOMAINS** | **LSTDOM 132B** | **0x5A** | **146720** | **007352-007354** | **007352** | none |

DELDOM's success at `007351` jumps to the command loop `010613`, so it does not fall
through into the LSTDOM call - two separate commands (PROVEN).

## How it fits ND-500 init

`LIST-STANDARD-DOMAINS` is an operator inspection command showing the defined standard
domains; the domain family (DEFDOM 127B define / 130B start / DELDOM 131B delete /
LSTDOM 132B list) manages the ND-500 program environments.

## Unknown / inferred

- **INFERRED (role)**: `002673` = error reporter, `010613` = command loop. Entry
  addresses and reachability PROVEN.
- **INFERRED**: operator keyword follows the subfunction purpose; bank-2 command-string
  table not consulted.
- **PROVEN**: LSTDOM takes no parameters (no `STx ,X` before the call at 007352).
